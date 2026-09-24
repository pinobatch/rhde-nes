RHDE: Furniture Fight
=====================

This is a design document for the 2014 game *RHDE: Furniture Fight*
(aka *Rouketopolemos Home Decorating Edition*) to support an enhanced
remake on another platform.

Related documents:

- [Algorithms](./gdd_algorithms.md)
- [Phases of play](./gdd_phases.md)

Player state
------------

Properties of each player that persist across phases take 57 bytes
apart from the map:

- Chosen character (1)
- Money (2)
- Rounds won (1)
- Whether the player last the last round, to show grass color (1)
- Piece deal depth (1)
- Piece outcomes ordered by recency (number of polyominoes + number
  repeated, currently 22)
- Furniture inventory (number of furniture types, currently 17)
- Width of each 2-row-tall strip of player's territory
  (map height / 2, currently 12)

To describe: State specific to each phase

Single-player
-------------

The first thing I want to make is a short single-player campaign that
acts as a tutorial.

### Scenario 1

Start with a bed in player 1's house, a silo in stock, and enough
money to buy a chair.  Computer-controlled player 2 has a bed and
table in the house.  An untimed furnish phase begins:

Player 2's window displays:

    Order a chair
    from the Shop.

Once player 1 owns a chair, player 2's window displays:

    Go to Stock
    and put it in
    your house.
    Then put the
    missile silo
    outside.

Once inventory is empty, untimed combat begins:

    Face your silo
    and blow holes
    in the other
    house's wall.

After three missiles connect:

    There's a table
    in that house.
    Bring it back.

### Scenario 2

- There's an abandoned shack across the road.  Grab a bat, use it on
  the fence and the door, and bring back what's inside.
- There's another shack.  Place a silo and fire a rocket to break in.

### Others' suggestions

- lidnariq suggested: "Here's two roamers, get them to the other side
  of the screen at approximately the same time"
