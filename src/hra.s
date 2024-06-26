;
; RHDE house rating algorithm
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "global.inc"

; I think I know why Nintendo called it Happy Room Academy: it has
; the same initials as "house rating algorithm".
;
; So these rules apply to each cell on a player's side.
;
; If not a flower or the top left of an indoor furni: 0
; For a flower: 3 points if it borders a wall; 2 points otherwise
; For an indoor furni:
;   Add 7 points.
;   Mark this furniture type as present
;   If ficus, add 3 more for touching a corner (both a wall on the
;     north/south and one on the east/west) or 2 points for otherwise
;     touching a wall.
;   If rug, add 3 more for touching a door.
;   If bookcase, add 3 more for touching a wall.
;   If furniture is member of a group:
;     Search for other members of this group nearby (2-cell margin on
;       each side). Add 5 for each other furni type found.
; When done scanning, add 25 points for each group that has
;   all types present.
; Groups are
;   [table, chair]
;   [sofa, bookcase]
;   [fridge, oven, trashcan]
;   [toilet, bathtub, sink]
;
; To scan for same-group furniture:
; group_items = [0] * 3
; doublecount_row = $00
; groupfound = False
; for groupoffset, grouplen in group_defs:
;   if 0 <= cur_furni - groupoffset <= grouplen:
;     groupfound = True
; if not groupfound:
;   return
; group_items[cur_furni - groupoffset] = 1
;
; left = max(3, cursor_x) - 2
; top = max(2, cursor_y) - 2
; right = min(26, cursor_x) + 3
; bottom = max(FIELD_HT - 3, cursor_y) + 3
; for y in xrange(top, bottom):
;   fieldptr = seek_fielddata_row(y)
;   doublecount_tile = doublecount_row
;   for x in xrange(left, right):
;     t = fieldptr[x]
;     if $52 <= t < $80:
;       offset, cur_furni = find_furni_tile_a(t)
;       if (offset & doublecount_tile) == 0 and cur_furni < NUM_FURNIS:
;         cur_furni = find_furni_main_rot(cur_furni) - groupstart
;         if 0 <= cur_furni < grouplen:
;           group_items[cur_furni] = 1
;     doublecount_tile |= $01
;   doublecount_row = $10
; fieldptr = get_top_left_corner(cursor_x[0], cursor_y[0])
; return (sum(group_items) - 1) * 5
;
; Worked example of original algorithm in docs/scoring_changes.md

HRA_MAX_GROUP_SIZE = 3
owned_items     = form_pageitems
placement_bonus = owned_items + NUM_FURNIS_NONWEAPON
proximity_bonus = placement_bonus + NUM_FURNIS_NONWEAPON
bonuses_end     = proximity_bonus + NUM_FURNIS_NONWEAPON
group_items    = bonuses_end
NEAR_WE_WALL   = $01
NEAR_NS_WALL   = $02
NEAR_DOOR      = $04
cur_furni      = cur_piece_lo+0
cur_furni_size = cur_piece_lo+1

PTS_PER_INDOOR = 7
PTS_PER_FLOWER = 2
PTS_PER_PROXIMITY = 5
FULL_GROUP_BONUS = 25

PROX_RADIUS = 2

; Tiles $52-$5F are furniture of height 1
; Tiles $60-$6F are the top half of double-height furniture
; Tiles $70-$7F are the bottom half of double-height furniture
INDOOR_FURNI_MIN_TILE = $52
INDOOR_FURNI_TOP_HALVES_MAX_TILE = $6C
INDOOR_FURNI_MAX_TILE = $7C

;;
; Rates one house.
; @param enclose_turn player to rate
; @return $0E-$0F: rating
.proc rate_furni
row_xend = cursor_x+1
itemdatalo = $00
ratinglo = $0E
ratinghi = $0F

  ; Initialize the rating
  lda #0
  sta cursor_y
  sta ratinglo
  sta ratinghi

  ; Clear counts and bonuses for each furni type
  ldx #bonuses_end - 1 - owned_items
  clrpresence:
    sta owned_items,x
    dex
    bpl clrpresence

  do_row:
    ; Establish the boundaries of this side
    lda cursor_y
    lsr a
    tay
    lda #RIGHTMOST_X + 1
    ldx enclose_turn
    bne :+
      lda xmax_1p,y
      asl a
    :
    sta row_xend
    lda #LEFTMOST_X
    cpx #0
    beq :+
      lda xmin_2p,y
      asl a
    :
    sta cursor_x
    ldx #0
    jsr get_piece_top_left_corner

    ; at this point Y is the starting X coordinate
    do_tile:
      ldy #33
      lda (fieldlo),y
      cmp #$52
      bcs maybe_indoor_furni
      and #%11111110
      eor #TILE_FLOWERS
      bne skip_tile

      ; Flower: Add 2, plus one more if touching a wall
      ;lda #0  ; guaranteed after EOR BNE
      sta cur_furni_size
      jsr find_walls_around_size
      and #NEAR_WE_WALL|NEAR_NS_WALL
      cmp #1
      lda #0
      adc #PTS_PER_FLOWER
      bne add_points

    maybe_indoor_furni:
      ; cur_furni, offset, itemdata = find_furni_tile_a(tile_id)
      ; if cur_furni < 0 or offset != 0: continue
      ; cur_furni_size = itemdata.size
      ; if not (cur_furni_size & SHOPITEMS_INDOOR): continue
      ; cur_furni = find_furni_main_rot(cur_furni)
      ; if cur_furni >= NUM_FURNIS_NONWEAPON: continue
      jsr find_furni_tile_a
      bmi skip_tile
      ora #$00
      bne skip_tile  ; disregard tiles other than top left corner
      sty cur_furni
      ldy #SHOPITEMS_SIZE
      lda (itemdatalo),y
      sta cur_furni_size
      and #SHOPITEMS_INDOOR
      beq skip_tile  ; if not indoor, fail
      lda cur_furni
      jsr find_furni_main_rot
      cmp #NUM_FURNIS_NONWEAPON
      bcs skip_tile  ; if weapon, fail
      sta cur_furni

      ; This is what we must change to delay calculation
      ; to deter toilet spam.  See scoring_changes.md
      jsr hra_score_furni_placement
      jmp skip_tile
    add_points:
      clc
      adc ratinglo
      sta ratinglo
      bcc skip_tile
      inc ratinghi
    skip_tile:
      inc fieldlo
      inc cursor_x
      lda cursor_x
      cmp row_xend
      bcc do_tile
    inc cursor_y
    lda cursor_y
    cmp #FIELD_HT
    bcc do_row

  ; Cap the quantity of each furniture item to discourage hoarding
  ldx #NUM_FURNIS_NONWEAPON-1
  cap_loop:
    txa
    jsr seek_to_furni_a
    ldy #SHOPITEMS_SIZE
    lda (itemdatalo),y
    and #SHOPITEMS_QTY_CAP_MASK
    cmp owned_items,x
    bcs :+
      sta owned_items,x
    :
    dex
    bpl cap_loop

  ; Award 7 points for each item under the cap
  lda owned_items+0
  clc
  ldx #NUM_FURNIS_NONWEAPON-1
  sum_owned_loop:
    adc owned_items,x
    dex
    bne sum_owned_loop
  ldy #PTS_PER_INDOOR
  jsr mul8
  pha
  lda $0000
  clc
  adc ratinglo
  sta ratinglo
  pla
  adc ratinghi
  sta ratinghi

  ; Award placement and proximity bonuses per type
  ldx #bonuses_end-placement_bonus-1
  placement_bonus_loop:
    lda placement_bonus,x
    clc
    adc ratinglo
    sta ratinglo
    lda #0
    adc ratinghi
    sta ratinghi
    dex
    bpl placement_bonus_loop


  ; Add bonuses for having full sets of each group
  ldy #(HRA_NUM_GROUPS - 1) * 2
  group_loop:
    lda group_defs+1,y
    sta row_xend
    ldx group_defs,y
    group_member_loop:
      lda owned_items,x
      beq skip_group
      inx
      dec row_xend
      bne group_member_loop
    lda #FULL_GROUP_BONUS
    clc
    adc ratinglo
    sta ratinglo
    bcc skip_group
      inc ratinghi
    skip_group:
    dey
    dey
    bpl group_loop
  rts
.endproc

.segment "RODATA"
group_defs:
  .byte item_table, 2   ; table, chair
  .byte item_sofa, 2    ; sofa, bookcase
  .byte item_fridge, 3  ; fridge, oven, trashcan
  .byte item_sink, 3    ; sink, toilet, bathtub
HRA_NUM_GROUPS = (* - group_defs) / 2

.segment "CODE"

;;
; Finds walls
; @param fieldlo square up and to the left of furniture
; @param cursor_y (must be consistent with fieldlo)
; @param cur_furni_size size bits of furni
.proc find_walls_around_size
skip_n = $08  ; <0: skip the north side of x_origin
x_origin = $09
y_origin = $0A
cur_side = $0B  ; what to OR into walls_found if a wall is found
walls_found = $0C

  lda #0
  sta skip_n
  sta walls_found

  ; Find Y origin (offset of cell to the right of the lower right
  ; corner of a furni)
  lda #34
  bit cur_furni_size
  bpl :+
    lda #66  ; Tall: add a row
  :
  bvc :+
    ora #1  ; Wide: add a column
  :
  sta y_origin

  ; Find X origin (offset of cell below the lower right corner
  ; of a furni)
  clc
  adc #31  ; Left and down
  sta x_origin

  ; If Y = 0, skip north side
  ldy cursor_y
  bne skip_n_not_top_row
    dec skip_n
    bne skip_n_determined
  skip_n_not_top_row:

    ; If Y + (furni height - 1) >= (field height - 1), skip south side
    bit cur_furni_size
    bpl :+
      iny
    :
    cpy #FIELD_HT-1
    bcc skip_n_determined
    dec skip_n
    ; Move the south side pointer (x_origin) to the north side and
    ; skip the north side.  This is cheaper in the inner loop than
    ; separate logic to skip the north and south sides.
    lda x_origin
    and #%00011111
    sta x_origin
  skip_n_determined:

  ; Check the west and east sides
  lda #NEAR_WE_WALL
  sta cur_side
  ldy y_origin
  we_loop:
    jsr handle_tile  ; Y = east side
    tya
    and #%11100000
    tay
    jsr handle_tile  ; Y = west side
    cpy #64    ; If not on the bottom row
    bcc we_done
    lda y_origin
    sec
    sbc #32
    sta y_origin
    tay
    bcs we_loop
  we_done:

  ; Check the north and south sides
  lda #NEAR_NS_WALL
  sta cur_side
  ns_loop:
    ldy x_origin
    jsr handle_tile  ; Y = south side (or north if along S wall)
    tya
    and #%00011111
    tay
    bit skip_n
    bmi :+
      jsr handle_tile  ; Y = north side
    :
    cpy #2    ; If not on the left column
    bcc ns_done
    dec x_origin
    bcs ns_loop
  ns_done:

  lda walls_found
  rts

handle_tile:
  lda (fieldlo),y
  and #%11111110
  cmp #CONNBLKS_DOORWE
  bne not_door
    lda #NEAR_DOOR
    bne have_wall_found
  not_door:
    and #%11110000
    cmp #CONNBLKS_ROW
    bne not_wall_either
    lda cur_side
  have_wall_found:
    ora walls_found
    sta walls_found
  not_wall_either:
  rts
.endproc

;;
; Counts this item and checks for placement and proximity bonuses.
; @param cur_furni identity of furni, rotated to home position
; @param cur_furni_size size of original unrotated furni
; @param cursor_x, cursor_y, fieldlo positioned at furni's top left
.proc hra_score_furni_placement
  ; Count quantity of each item and mark as present for group bonus
  ldx cur_furni
  inc owned_items,x
  bne :+
    dec owned_items,x
  :

  ; Placement bonuses for indoor furni based on wall and door proximity
  cpx #item_ficus
  bne not_ficus
    jsr find_walls_around_size
    and #NEAR_WE_WALL|NEAR_NS_WALL
    beq have_placement_bonus  ; No walls: 0 pts
    cmp #1
    bne have_placement_bonus  ; NS: 2; NS + WE: 3
    asl a
    bne have_placement_bonus  ; WE: 2
  not_ficus:

  cpx #item_rug
  bne not_rug
    jsr find_walls_around_size
    and #NEAR_DOOR
    beq have_placement_bonus
  wall_prox_give3:
    lda #3
    bne have_placement_bonus
  not_rug:

  cpx #item_bookcase
  bne wall_prox_give0
    jsr find_walls_around_size
    and #NEAR_WE_WALL|NEAR_NS_WALL
    bne wall_prox_give3
  wall_prox_give0:
    lda #0
  have_placement_bonus:
  ldx cur_furni
  cmp placement_bonus,x
  bcs :+
    sta placement_bonus,x
  :

  ; If the furni is in a group, scan for nearby furni to assign a
  ; furni-to-furni proximity bonus

tile_x = $08
tile_y = cursor_y + 1
left = $09
right = $0A
bottom = $0B
groupstart = next_piece+0
grouplen = next_piece+1

; For speed, ignore bottom halves except on the first scanned row
ignore_bottomhalf = cur_piece_hi+1

  lda #INDOOR_FURNI_MAX_TILE+1
  sta ignore_bottomhalf
  lda #0
  ldy #HRA_MAX_GROUP_SIZE-1
  clrgrp:
    sta group_items,y
    dey
    bpl clrgrp

  ; Find group that contains furni X
  ldy #(HRA_NUM_GROUPS - 1) * 2
  findgrp:
    lda group_defs,y
    sta groupstart
    lda group_defs+1,y
    sta grouplen
    txa
    sec
    sbc groupstart
    cmp grouplen
    bcc found_group
    dey
    dey
    bpl findgrp
  rts  ; No group, no proximity bonus update

found_group:
  tay
  lda #PTS_PER_PROXIMITY
  sta group_items,y

  ; Calculate bounding box of search
  ; 2 to left of left side
  lda cursor_x
  cmp #LEFTMOST_X + PROX_RADIUS
  bcs :+
    lda #LEFTMOST_X + PROX_RADIUS + 1  ; plus 1 for carry clear
  :
  sbc #PROX_RADIUS
  sta left

  ; 2 to right of right side (incl. 1 normally plus 1 more if wide)
  lda cur_furni_size
  and #SHOPITEMS_WIDE
  cmp #1  ; CF=1 if wide
  lda cursor_x
  adc #0
  cmp #RIGHTMOST_X - PROX_RADIUS
  bcc :+
    lda #RIGHTMOST_X - PROX_RADIUS - 1  ; minus 1 for carry set
  :
  adc #PROX_RADIUS + 1
  sta right

  ; 2 above top
  lda cursor_y
  cmp #PROX_RADIUS
  bcs :+
    lda #PROX_RADIUS + 1  ; plus 1 for carry clear
  :
  sbc #PROX_RADIUS
  sta tile_y

  ; 2 below bottom (incl. 1 normally plus 1 more if tall)
  lda cur_furni_size
  .assert SHOPITEMS_TALL = $80, error, "will need to use and/cmp instead"
  asl a  ; CF=1 if wide
  lda cursor_y
  adc #0
  cmp #FIELD_HT - 1 - PROX_RADIUS
  bcc :+
    lda #FIELD_HT - 1 - PROX_RADIUS - 1  ; minus 1 for carry set
  :
  adc #PROX_RADIUS + 1
  sta bottom

  rowloop:
    lda tile_y
    jsr seek_fielddata_row_a
    ldy left
    tileloop:
      sty tile_x
      lda (fieldlo),y
      cmp #$52  ; only test tiles that are likely furni
      bcc skiptile
      cmp ignore_bottomhalf
      bcs skiptile
      jsr find_furni_tile_a
      bmi skiptile  ; Y=furni id, A=offset in YYYYXXXX
      ; At this point we have furniture Y.  Find its main rotation
      ; and see if it's one of the others in this group.
      tya
      jsr find_furni_main_rot
      sec
      sbc groupstart
      cmp grouplen
      bcs skiptile
      tay
      lda #PTS_PER_PROXIMITY
      sta group_items,y

    skiptile:
      ldy tile_x
      iny
      cpy right
      bcc tileloop
    ; on the next row, reject bottom halves of furniture
    lda #INDOOR_FURNI_TOP_HALVES_MAX_TILE + 1
    sta ignore_bottomhalf
    inc tile_y
    lda tile_y
    cmp bottom
    bcc rowloop

  ; now that we've scanned the surroundings,
  ; put the playfield pointer back on the cursor
  ldx #0
  jsr get_piece_top_left_corner

  ; count other item types encountered: 5*max(0,sum(group_items)-1)
  lda #<-PTS_PER_PROXIMITY
  .repeat ::HRA_MAX_GROUP_SIZE, I
    clc
    adc group_items+I
  .endrepeat
  bmi is_not_increase  ; should this ever happen?

  ; Chair bonus for being near a table is cumulative with a cap.
  ; Others are plain maximum.
  ldx cur_furni
  cpx #item_chair
  bne have_noncumulative_bonus
    clc
    adc proximity_bonus,x
    cmp #2 * PTS_PER_PROXIMITY
    bcc have_noncumulative_bonus
    lda #2 * PTS_PER_PROXIMITY
  have_noncumulative_bonus:
  cmp proximity_bonus,x
  bcc is_not_increase
    sta proximity_bonus,x
  is_not_increase:
  rts
.endproc
