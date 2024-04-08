RHDE RAM map
============

One build of *RHDE* as of March 2024 has a sound driver in a separate
bank to be assembled separately.  Its RAM map is as follows:

- $0000-$000F: Local variables
- $0010-$00CF: Segment `ZEROPAGE` (capacity 192 bytes)
- $00D0-$00F7: Sound state (mostly pointers)
- $00F8-$00FF: Controller state, soft reset detection, NMI counter
- $0100-$01BF: VRAM transfer buffer
- $01C0-$01FF: Stack
- $0200-$02FF: Shadow OAM
- $0300-$05FF: State of 28x24-cell playfield, 1 byte per cell
- $0600-$077F: Segment `BSS` (capacity 384 bytes)
- $0780-$07E4: Sound state
- $07E5-$07FF: Reserved for sound state

Segments `ZEROPAGE` and `BSS` total 576 bytes, of which the game uses
about 75 percent.

- `ZEROPAGE`: 106 bytes
    - scoring and game progress: 16
    - battle phase unit state: 42 (7 per unit for 12 units)
        - furnish phase menu state: 18 (overlapped)
    - other battle phase state: 11
    - build phase current and next pieces: 10
    - VRAM transfer shape: 8
    - CHR decompression pointers: 6
    - indices into playfield, OAM, and player state: 4
    - Control Pad autorepeat: 4
    - random number generator state: 2
    - debug value to print: 2
    - wall connection task progress: 1
- `BSS`: 328 bytes
    - build phase piece shuffling: 46
    - build phase wall enclosure task state: 96
        - furnish phase menu page contents: 16 (overlapped)
        - battle phase silo projectile positions: 96 (overlapped)
    - wall enclosure task progress: 5
    - wall enclosure unused: 3
    - shape of road between players: 24
    - furniture inventory: 34
    - battle phase cannon state: 48
    - battle phase damaged doors: 32 (2 per door for 16 door tiles)
    - battle phase items dropped on ground: 24 (3 per item for 8 items)
    - string formatting buffer: 16

This leaves 142 bytes to add anything new, such as better pathfinding
or player weapons.
