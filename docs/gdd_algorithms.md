Algorithms in RHDE
==================

Many of these algorithms are shared by more than one phase.

House rating
------------

The house rating algorithm considers the garden score, population,
placement bonus, proximity bonus, and full groups.

Population encourages owning a variety of different furniture.
Each type of item has a cap to discourage, say, hoarding toilets.
Placement bonus encourages placing certain items near walls when
appropriate.  Proximity bonus encourages placing related items,
such as a table and chairs, next to each other.  Full group bonus
encourages completing sets of related items.

Let population, placement bonus, and proximity bonus be arrays of
length `NUM_FURNIS_NONWEAPON`, initialized to 0.  Then for each tile
in the player's territory:

- If it is a flower, add 2 points to the garden score, plus one more
  if touching a wall.
- If it is not the top left corner of an indoor furniture item,
  skip this cell.
- Find the item's primary rotation.
- If the item is a weapon, skip this cell.
- Add 1 to the item's population, unless that would put it over 255.
- If this item is a ficus, and it is next to a wall, increase the
  placement bonus of ficus to 2.
- If this item is a ficus, and it is next to a wall on both the top
  or bottom and on the left or right, increase the placement bonus
  of ficus to 3.
- If this item is a bookcase, and it is next to a wall, increase the
  placement bonus of bookcase to 3.
- If this item is a rug, and it is next to a door, increase the
  placement bonus of rug to 3.
- Find which group this item is in.  If none, skip this cell.
- Let proximity rectangle be the edges of the item, extended outward
  by 2 cells on each side, clipped to the map extent.
- Let nearby group items be an array of length the maximum size of a
  group, initialized to 0.
- For each cell within the proximity rectangle that is an indoor
  furniture item, find the item's primary rotation.  If it is within
  the group, set its entry in nearby group items to 5.
- Take the sum of nearby group items.  If this item is a chair,
  increase chair's proximity bonus by the sum to a maximum of 10.
  Otherwise, increase this item's proximity bonus to the sum.

Let the score for population be the sum over all item types of
the following:  Multiply the lesser of the item type's cap and its
population by 7.

Let the score for placement be the sum of all items' placement bonus.

Let the score for proximity be the sum of all items' proximity bonus.

Let the score for full groups be the sum over all groups of
the following: If all items in the group have population more than
zero, add 25.

Let the player's total HRA score be the sum of the scores for garden,
population, placement, proximity, and full groups.  Let the winner of
a round be the player with the higher total HRA score.

Tunable:

- Garden score per flower
- Population cap per item type
- Population weight
- Individual item types' placement bonuses
- Proximity distance
- Chair-to-table proximity cap
- Proximity weight
- Full group weight

Piece generation
----------------

Each player's piece generator implements a least recently used queue through a vector of outcomes and a deal depth variable. As a game proceeds, more complex pieces are added into play. It responds to messages "restart" and "pull an item".

To restart the generator:

1. Set the outcome vector's contents equal to the initial outcome vector.
2. Set the deal depth equal to the initial deal depth.

To pull an item:

1. Let the chosen index be a uniform random index early in the queue.
2. Remove and remember the outcome at the chosen index.
3. Let the tail index be truncated deal depth - 1.
4. Move all outcomes in the queue from chosen index + 1 through tail index to the previous element.
5. Replace the removed outcome at the tail index.
6. If deal depth is less than the length of the outcome vector, increase deal depth by a fraction.
7. Return the removed outcome.

Valid pieces are one-sided pentominoes and lighter polyominoes, meaning they have 1 to 5 square cells joined at the edges. They are also restricted to fit in a 3 by 3 cell square (excluding N5 and anything containing I4) and not to contain the O tetromino (no O4 or P5).

- Sub-tetrominoes: Monomino, I2, I3, L3
- Tetrominoes: J4, L4, T4, S4, Z4
- Pentominoes: U5, X5, S5, Z5, V5, T5, F5, R5, W5

```
Sub-pentominoes:
. . .   . . .   . . .   . % .   % . .   . . %   . % .   . % %   % % .
. % .   . % %   % % %   . % %   % % %   % % %   % % %   % % .   . % %
. . .   . . .   . . .   . . .   . . .   . . .   . . .   . . .   . . .

Pentominoes:
% . %   . % .   . % %   % % .   % . .   . % .   % . .   . . %   % . .
% % %   % % %   . % .   . % .   % . .   . % .   % % %   % % %   % % .
. . .   . % .   % % .   . % %   % % %   % % %   . % .   . % .   . % %
```

Tunable parameters:

- Range of chosen indices (we use 0 to 3)
- Composition and order of initial outcomes (we use all four sub-tetrominoes twice, then all tetrominoes, then all pentominoes)
- Initial rotation state of each outcome (we use flat on the bottom)
- Initial deal depth, or how many pieces are in the queue at first
  (we use the first 7)
- Deal depth increase rate, or how fast the more complex pieces are added to the LRU queue (we use one increase every 2 pulls)

To increase rollback netcode stability, each player's piece generator can have a separate raw RNG stream as well.

Connection
----------

Apply this to each cell, left to right, top to bottom:

1. Skip any cell that is not a wall or door.
2. For a vertical door, if there is a wall or door to the left or
   right, turn it into a wall; otherwise turn it into a connected
   vertical door.
3. For a horizontal door, if there is a wall or door above or below,
   turn it into a wall; otherwise turn it into a connected horizontal
   door.
4. If the cell to the left is a wall or door:
    - If this cell is a wall, set its left connection.
    - If the cell to the left is a wall, set its right connection.
5. If the cell above is a wall or door:
    - If this cell is a wall, set its top connection.
    - If the cell above is a wall, set its bottom connection.

New in the remake: A door with neighbors in both axes should become
a wall.

Flooding for enclosure and lighting
-----------------------------------

The goal of build phase is to enclose living space for your units.

- A non-wall cell is "enclosed" if there is no path from the map edge
  through 8-way connections without crossing a wall or door.
- An enclosed cell is "lit" if it is enclosed and a path from the map
  edge crosses only doors, not walls.

A background task calculates which cells are enclosed or which are
lit.  It involves these steps:

1. Create a bitmap "walls" from the map, where 0 is a wall and
   1 is any other cell.  Door cells count as walls for enclosure or
   others for lighting.
2. Create a second bitmap "seed" representing reached cells.  The top
   and bottom rows match walls, and rows in between are 1 to the left
   of the leftmost 0 in walls and to the right of the rightmost 0 in
   walls.
3. Extend 1s in seed by one cell to the left and right.
4. Extend 1s in seed by one cell up and down.
5. Set all bits that were 0 in walls to 0 in seed.
6. Set all bits that were 1 in seed to 0 in walls.
7. If any seed remains, go to 3.

During this process, walls holds 0 for what is known not to be the
interior, and seed represents a wave of propagation of exterior.
At the end, walls holds 1 for things within the walls and 0 for the
walls plus everything outside them.

- Before build phase, run enclosure and replace outside floor with
  grass and move outside indoor furniture to inventory.
- During build phase after placing each piece, and during trim phase
  after removing a wall, run enclosure and replace inside grass with
  dark floor.
- After build phase, run enclosure and move inside outdoor furniture
  to inventory.
- Before trim phase, and after each action in trim phase, run
  lighting and replace outside floor with lit floor and inside floor
  with dark floor.

Enclosure controls where furniture can be placed.

- Outdoor furniture can be placed in grass cells.
- Indoor furniture can be placed in lit floor cells.

### Nearly enclosed

Some similar games can detect whether an area is nearly enclosed.
This requires more research.

