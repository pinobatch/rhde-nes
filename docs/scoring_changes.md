House rating algorithms
=======================

We want to reward the player after each furnish round for
accumulating the necessities of life within their house and
for putting furniture items in fitting places relative to
one another and to the walls and doors.

The original algorithm
----------------------

The 2014 compo edition of RHDE followed these steps:

```
Set side's HRA score to 0
For each cell:
  If the cell is a flower:
    Check the sides for walls and doors
    Add 2 points
    Add 1 point more if a wall was found
  If not the top left corner of an indoor furni:
    continue
  If a fence or silo:
    continue
  Add 7 points
  Mark this furniture type as being in the house
  If ficus:
    Check the sides for walls and doors
    Add 3 points if touching a corner (walls on both N/S and E/W)
    Add 2 point if touching only either N/S or E/W wall
  If rug:
    Check the sides for walls and doors
    Add 3 points if touching a door
  If bookcase:
    Check the sides for walls and doors
    Add 3 points if touching a wall
  If the furniture is in a group:
    Search within a margin of 2 cells on each side for up to two
      other furnis in the same group.  Cells that aren't left
      count only if on left column of margin; cells that aren't
      top count only on top row of margin.
    Add 5 points for one or 10 points for two or more.
    (This will end up double-counting most furniture.  For example,
     a table gives 5 points for being by a chair, and a chair 5 for
     being by a table.)
For each group:
  If all items in the group are present:
    Add 25 points
```

Furniture groups:

1. table, chair
2. sofa, bookcase
3. fridge, oven, trashcan
4. toilet, bathtub, sink

Example of original algorithm
-----------------------------

Inside furniture: 20*7 = 140 points
Ficus: 2, both touching corners, 6 points
Rugs: 2, both against a door, 6 points
Flowers: 3, touching wall so 9 points
Bookcase: 3, touching wall so 9 points

- Group 1: All present, 25 pts
    - Chair: 5 pts extra for being by table
    - Table: 5 pts extra for being by chair
  Total: 10+25=35

- Group 2: All present, 25 pts  
  All items within range of each other, worth 40 points total  
  Total: 25+40=65

- Group 3: All present, 25 pts  
  All items within range of each other, worth 30 points total  
  Total: 25+30=55

- Group 4: All present, 25 pts  
  All items within range of each other, worth 30 points total  
  Total: 25+30=55

House total: 380 points

Related pseudocode
------------------

```
;;
; Finds wall and door tiles next to furni.
; @param fieldlo the square NW of a furni
; @param A the furni's size (7: tall, 6: wide)
; @param something to indicate whether to ignore N and S walls,
; for use with flowers next to N and S walls of playfield
find_walls_on_sides:
  Save furni's size.
  Set walls_found to 0.

  ; Check WE walls
  Set Y origin to 66 if tall or 34 if not.
  If wide, add 1 to Y origin.
  Set X origin to Y origin + 31.
  If skip south:
    ; If skipping the south side, check the north side
    ; instead of the south side.
    Set X origin to X origin & %00011111.
    Set skip north.

  Set cur_side to west and east.
  While Y origin >= 32:
    Y = Y origin
    Call handle_tile
    Y = Y origin & %11100000
    Call handle_tile
    Y origin -= 32

  Set cur_side to north and south.
  While (X origin & %00011111) >= 0:
    Y = X origin
    Call handle_tile
    If not skipnorth:
      Y = X origin & %00011111
      Call handle_tile
    X origin -= 1
  Return door_found, wall_we, wall_ns

  handle_tile:
    if fieldlo[Y] is a door:
      Add doors to walls_found.
    elif fieldlo[Y] is a wall:
      Add cur_side to walls_found.

;;
; Scans up to 2 cells away for other furniture in the same group.
; For speed, only furniture with tiles in $50-$7F is considered.
; Cells of a large furni other than the top left are considered
; only along the top row and left column of the search rectangle.
; @param groupmember1 One group member other than this furni.
; @param groupmember2 Second group member other than this furni.
; @param x, y where to start scanning, and how wide the furni is
; @return count Number of matched furni; capped at 2

To scan for other same-group furniture:
  groupfound = False
  for groupoffset, grouplen in groups:
    if 0 <= cur_furni - groupoffset <= grouplen:
      groupfound = True
  if not groupfound:
    return
  nearby_furni_found = [0] * 3
  nearby_furni_found[cur_furni - groupoffset] = 1

  left = max(3, cursor_x) - 2
  top = max(2, cursor_y) - 2
  right = min(26, cursor_x) + 3
  bottom = max(FIELD_HT - 3, cursor_y) + 3
  doublecount_row = $00
  for y in xrange(top, bottom):
    fieldptr = seek_fielddata_row(y)
    doublecount_tile = doublecount_row
    for x in xrange(left, right):
      t = fieldptr[x]
      if $52 <= t < $80:
        offset, cur_furni = find_furni_tile_a(t)
        if (offset & doublecount_tile) == 0 and cur_furni < NUM_FURNIS:
          cur_furni = find_furni_main_rot(cur_furni) - groupstart
          if 0 <= cur_furni <= grouplen:
            this_furni_found[cur_furni] = 1
      doublecount_tile |= $01
    doublecount_row = $10
  fieldptr = get_top_left_corner(cursor_x[0], cursor_y[0])
```

Anti-hoarding
-------------

One of our testers was spamming toilets and sinks to farm their
proximity bonus.  We should probably cap that to encourage players
to incorporate a larger fraction of the necessities of life and to
plant a garden.

The first pass will no longer tally score for furniture.  Instead,
it counts furniture in three arrays of length `NUM_FURNIS_NONWEAPON`
to score them afterward.

- `owned_items`: Number of items of each type, capped at 3 for bed or
  chair, 2 for ficus, rug, or bookcase, and 1 for others.
- `placement_bonus`: The highest-scoring placement for each type,
  such as rug at door, bookcase at wall, and ficus at wall or corner.
- `proximity_bonus`: For chairs, number within 2 cells of a table.
  For others, the maximum number of same-group item types encountered
  within 2 cells.

Because these arrays now encode a maximum score for each type,
they no longer reward hoarding the way a sum does.

Leave flower calculation (3 at wall, 2 otherwise) in place for now.

For each item of type cur_furni discovered in maybe_indoor_furni:

- Add 1 to `owned_items`.
- Update `placement_bonus` only if better than the existing one.
- Update `proximity_bonus` only if better than the existing one,
  except chair may increase from 1 to 2.

When finished, add these to the flower score:

- 7 for each owned item (max 161?)
- 5 for each proximity bonus (max 65?)
- The sum of the best placement bonus for each type (max 9)
- 25 for each group with all members having nonzero `owned_items`
  (max 100)

