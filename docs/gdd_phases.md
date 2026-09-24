Phases in RHDE
==============

A game of RHDE proceeds through several phases in a loop:
furnish, battle, build, and trim.

Furnish phase
-------------

Each player is in one of four states: inventory, shop, placement, or
pickup.

### Inventory

A pop-up window covers the lower third of the player's territory.
It shows two rows of four icons, one for each type of item in the
player's inventory, with a count below.

- Up: If on the second row, move to the first.  Otherwise, if not on
  the first page, move to the second row of the previous page.
  Otherwise, go to Pickup.
- Down: If on the first row, and there is a second row, move to the
  second row.  If on the second row, move to the first row of the
  next page, and if the page is empty, move back a page.  Either way,
  if past the end of the row, move to the end of the row.
- Left: If not on the first column, move to the previous column.
- Right: If cursor is on the last column or the last item on the
  first row of the page, go to Shop.  Otherwise, move to the next
  column, and if past the end of the row, move up.
- A Button: If at least one of this item is owned, go to Placement.
- Hold B Button and press A Button: If at least one of this item is
  owned, sell one for one-fourth of its purchase price, with
  one-half rounded up.

Sold items with zero quantity remain visible until the page changes.

### Shop

A pop-up window covers the lower third of the player's territory.
It shows four item descriptions and prices, with a picture of the
item that the cursor points to.  Items with a price higher than the
player's cash on hand are not listed.

- Up: If not on first row of this page, move to the previous row.
  Otherwise, if not on first page, move to the last row of the
  previous page.
- Down: If on last row of page, treat as Right.  Otherwise, move to
  the next row of this page.
- Left:
- Right: If this page is full, go to next page.  If blank, move to
  the last row of the previous page.
- Control Pad: Move to an item
- A Button: If the player has fewer than 99 of the item at the
  cursor and the player's cash is no less than the item's price,
  add 1 of the item to the player's inventory and subtract its price
  from the player's cash, and then wait for the item to animate
  moving upward into the inventory label of the pop-up's title bar.
- B Button: Go to Inventory

### Placement and pickup

The player's entire territory is shown.  The player can move a cursor
within the player's territory.  This cursor is shaped like an item in
placement or a hand truck in pickup.  Shifting behaves the same as in
build phase, and for kick purposes, an item moves as a 1x1, 1x2, 2x1,
or 2x2 block piece depending on its size.

- Control Pad: Move the cursor
- A Button in pickup: If over a furniture item, erase the item,
  add 1 of that item's primary rotation to inventory, and go to
  placement.
- A Button in placement: If the player has at least one of the item's
  item's primary rotation, and if all cells under it are grass (for
  outdoor items) or lit floor (for indoor items), subtract 1 of that
  item's primary rotation from the player's inventory and write the
  item to the map.  If the player has none of the item's primary
  rotation, go to Inventory.
- B Button: If in pickup, or the item does not rotate, or the B
  Button is autorepeating, go to Inventory.  Otherwise, change the
  piece to its next rotation and allow the B Button to autorepeat.

After placing an item, the item is deducted from inventory.  If
another item of the same type is owned, remain in Placement.
Otherwise, return to Inventory.

Battle phase
------------

The first three beds found in each player's territory each generate
one unit.

When a unit is focused:

- Control Pad: Move the unit's target cell.  The unit continuously
  walks toward its target cell, alternating horizontal and vertical
  steps.
- Hold B Button and press A Button:  Move player's focus to next unit
- A Button: If holding a bat, swing it.  Otherwise, if facing a silo,
  focus that silo.
- B Button: Pick up or drop an item.

When a player's unit steps onto the player's own territory while
holding an opponent's item, add that item to the player's inventory.

Build phase
-----------

During build phase, both players repair damage to their houses' walls
and expand them to enclose more territory.

At the start of build phase:

1. Replace connected wall and door with disconnected blocks.
2. Run the enclosure algorithm for each player.
3. Move all indoor furniture outside of enclosure to inventory.
3. Show the build phase banner.
4. Move each player's cursor to the center of the player's territory.
5. Pull each player's current and next pieces.
6. Start a countdown timer.

A piece centered on each player's cursor hovers over the player's
territory.  The next piece is drawn half-size, centered above the
current piece's 3×3-cell box.

- Control Pad: Shift the piece horizontally or vertically within the
  player's own territory by one cell, with autorepeat
- B Button: Rotate the piece clockwise by 90 degrees about the cursor
- A Button: Place the piece if all cells under it are empty space
  (grass or floor)

If any blocks of the piece lie outside the player's territory after
shifting or rotation, the piece is kicked vertically past the top
and bottom walls and then kicked horizontally past the side walls
and road.

When a piece is placed:

1. Change the cells within the piece from space into wall blocks
2. Schedule an enclosure scan per "Enclosure and lighting"
3. Move the next piece to the current piece
4. Pull the next piece per "Piece generation"
5. Snap the piece into the territory

When the timer expires, erase the next piece and give players a grace
period to place the current piece.

Tunable parameters:

- Autorepeat delay and speed (we use 200 ms and 30 Hz)
- Phase duration (we use 22 seconds)
- End grace period (we use 4 seconds)
- Spacing between current and next pieces

New in the remake: Forfeit match if no enclosure at the end of a
build phase

Trim phase
----------

At the start of trim phase:

1. Find walls with only 1 wall neighbor and remove them.
2. Calculate connection and enclosure for all areas.
3. Return unenclosed outdoor furniture and enclosed indoor furniture to the player's inventory.
4. Calculate lighting.
5. Show the trim phase banner.
6. Move each player's cursor to the center of the player's territory.
7. Start a countdown timer.

The player can move the cursor with autorepeat and press the place button to act on the cell under the cursor.

- **End trim:** If the wall has fewer than two connections, replace
  it with ground.
- **Corner trim:** If the wall has horizontal and vertical
  connections and a wall in their diagonally adjacent combined
  direction corresponding to each pair of horizontal and vertical
  connections, replace it with ground.
- **Horizontal door:** If the wall has walls to the left and right
  and passable empty space above and below, replace it with a
  horizontal door.
- **Vertical door:** If the wall has walls above and below and
  passable empty space to the left and right, replace it with a
  vertical door.

Trimming a wall disconnects adjacent walls and schedules an enclosure
and lighting check.  Placing a door schedules a lighting check.

Tunable parameter:

- Length of trim phase
