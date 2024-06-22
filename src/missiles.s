;
; RHDE missile silos
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

MAX_CANNONS = 12  ; Outdoor fixed projectile launching platforms ("silo")
MAX_ROCKETS = 8   ; Projectiles fired from "cannons"
CANNON_COOLDOWN_TIME = 25  ; times 0.1 s

; Because we don't need to run the flood fill CA in the battle phase,
; we can reuse its FIELD_HT*4 = 96 byte scratchpad for rocket state
; missile_subpx is in XXXXYYYY
; dir is in turn-fractions, updated occasionally; if it's >=128,
; the slot is available
cannon_missile_x     = floodscratch
cannon_missile_y     = cannon_missile_x + 2 * MAX_ROCKETS
cannon_missile_subpx = cannon_missile_y + 2 * MAX_ROCKETS
cannon_missile_dir   = cannon_missile_subpx + 2 * MAX_ROCKETS
cannon_missile_dstx  = cannon_missile_dir + 2 * MAX_ROCKETS
cannon_missile_dsty  = cannon_missile_dstx + 2 * MAX_ROCKETS
cannon_missile_end   = cannon_missile_dsty + 2 * MAX_ROCKETS
.assert cannon_missile_end - floodscratch <= 4 * FIELD_HT, error, "Rockets state doesn't fit in flood fill scratchpad"

.segment "BSS"
cannon_loc:           .res 2 * MAX_CANNONS  ; XXXXYYYY coords of center
cannon_cooldown_time: .res 2 * MAX_CANNONS

unit_missile_time:  .res 2 * MAX_UNITS  ; frames till shoot again
unit_missile_x:     .res 2 * MAX_UNITS
unit_missile_y:     .res 2 * MAX_UNITS
unit_missile_dir:   .res 2 * MAX_UNITS  ; in turn-fractions
unit_missile_subpx: .res 2 * MAX_UNITS  ; XXXXYYYY

player_state = cur_piece_hi

.segment "CODE"
.proc init_cannons

  ; Clear missiles fired from silos
  ldx #2 * MAX_ROCKETS - 1
  lda #$FF
rocketloop:
  sta cannon_missile_dir,x
  dex
  bpl rocketloop

  ; Clear unit missiles
  ldx #2 * MAX_UNITS - 1
unitloop:
  sta unit_missile_dir,x
  dex
  bpl unitloop

  ; Clear unused cannons' locations and cooldown times
  ldx #2 * MAX_CANNONS - 1
cannonloop:
  lda #$FF
  sta cannon_loc,x
  .assert CANNON_COOLDOWN_TIME < 128, error, "Cannon cooldown must be less than 12.8 s"
  lda #CANNON_COOLDOWN_TIME
  sta cannon_cooldown_time,x
  dex
  bpl cannonloop
  
  inx
  stx cur_turn
  jsr find_cannons
  inc cur_turn
find_cannons:

cur_y = $07  ; stores (Y-1)/2
cur_x = $06  ; stores X-1 (not divided by 2)
cannon_idx = $05 ; stores cur_turn + 2 * number of found cannons

  ldx cur_turn
  stx cannon_idx
  lda #14

rowloop:
  sta cur_x
  lda #<fielddata
  sta fieldlo
  lda #>fielddata
  sta fieldhi
  ldy #0
tileloop:

  ; Make sure that this tile belongs to this side
  sty cur_y
  lda cur_x
  lsr a
  cmp xmax_1p,y  ; C = side (0=left, 1=right)
  rol a
  eor cur_turn   ; A.D0 = side (0=match, 1=opposite) 
  lsr a
  bcs nexttile

  ; Make sure the tile is part of a cannon
  ldy cur_x
  lda (fieldlo),y
  eor #$7F
  and #$EF
  cmp #$03
  bcs nexttile

  ; So there is a cannon here
  lda cur_x
  asl a
  asl a
  asl a
  ora cur_y
  ldy cannon_idx
  sta cannon_loc,y
  lda #<-1
  sta cannon_cooldown_time,y
  iny
  iny
  sty cannon_idx
  cpy #2 * MAX_CANNONS
  bcs found_enough_cannons
nexttile:
  lda fieldlo
  clc
  adc #64
  sta fieldlo
  bcc :+
  inc fieldhi
:
  ldy cur_y
  iny
  cpy #FIELD_HT/2
  bcc tileloop
  
  ; Go to next column
  clc
  lda cur_x
  adc search_xstep,x
  cmp #30
  bcc rowloop
found_enough_cannons:
  rts
.pushseg
.segment "RODATA"
search_xstep:  .byte <-2, 2
.popseg
.endproc

;;
; Sets N iff no rockets are in flight.
.proc any_cannon_missiles_left
  ldx #2 * MAX_ROCKETS - 1
loop:
  lda cannon_missile_dir,x
  bpl found_one
  dex
  bpl loop
found_one:
  rts
.endproc

;;
; Gets the position in tiles of the top left corner of cannon Y.
; @param Y id of cannon
; @return 0=x tile, Y=x tile, 1=y tile 
.proc get_cannon_y_pos
pt_x = 0
pt_y = 1

  ; cannon_loc coordinates pointers are low-res: XXXXYYYY. They point
  ; to ONE tile within the cannon, which might not be the top left.
  ; Move to the left side of the cannon.
  lda cannon_loc,y
  and #$0F
  asl a
  sta pt_y
  jsr seek_fielddata_row_a
  lda cannon_loc,y
  and #$F0
  lsr a
  lsr a
  lsr a
  
  ; cannon_loc coordinates pointers are low-res. They point to ONE
  ; tile within the cannon, which might not be the top left.
  ; Move to the left side of the cannon.
  tay
  lda (fieldlo),y
  and #$0F
  cmp #$0E
  bne :+
  dey
:
  sty pt_x
  ; Move to the top side of the cannon.
  lda (fieldlo),y
  and #$10
  beq :+
  dec pt_y
:
  rts
.endproc

.proc control_cannon
  lda new_keys,x
  and #KEY_B
  beq notB
  lda #0
  sta player_state,x
  rts
notB:
  lda #KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT
  and das_keys,x
  sta das_keys,x
  jsr autorepeat
  
  lda new_keys,x
  lsr a
  bcc notRight
  inc cursor_x,x
  lda cursor_x,x
  cmp #29
  bcc notUp
  dec cursor_x,x
  bne notUp
notRight:

  lsr a
  bcc notLeft
  dec cursor_x,x
  bne notUp
  inc cursor_x,x
  bne notUp
notLeft:

  lsr a
  bcc notDown
  inc cursor_y,x
  lda cursor_y,x
  cmp #FIELD_HT
  bcc notUp
  dec cursor_y,x
notDown:

  lsr a
  bcc notUp
  dec cursor_y,x
  bpl notUp
  lda #0
  sta cursor_y,x
notUp:
  jsr find_closest_cannon
  cpy #2 * MAX_CANNONS
  bcc have_cannon
  lda #$FF
  sta player_cannon_x,x
  rts
have_cannon:

  ; at this point Y is a cannon ID
  jsr get_cannon_y_pos
  sty player_cannon_x,x
  lda 1
  sta player_cannon_y,x

  lda new_keys,x
  bmi fire_cannon_missile
  rts
.endproc

;;
; Fires a missile from player_cannon_x/y to cursor_x/y.
.proc fire_cannon_missile
  ; Check if still facing a cannon (it might have been destroyed)
  ; TO DO
  jsr furni_in_front_of_unit
  jsr is_unit_by_cannon
  txa
  and #$01
  tax
  cpy #30  ; still on screen?
  bcc still_by_cannon
  lda #0
  sta player_state,x
nope:
  rts
still_by_cannon:

cannon_idx = $05

  ; Find a free rocket slot and cannon
  jsr find_cannon_missile_slot
  bcs nope
  sty cannon_idx
  jsr find_closest_cannon
  cpy #2 * MAX_CANNONS
  bcs nope

  ; At this point, we have a free cannon (#Y) and a rocket slot.
  ; So mark this cannon as used.
  lda #CANNON_COOLDOWN_TIME
  sta cannon_cooldown_time,y
  ; TO DO: Mark the cannon's tile as used and request a side update

  ldy cannon_idx
  txa
  beq :+
  lda #16
:
  sta cannon_missile_dir,y
  lda #0
  sta cannon_missile_subpx,y
  lda player_cannon_x,x
  sec
  adc #0
  asl a
  asl a
  asl a
  sta cannon_missile_x,y
  lda player_cannon_y,x
  sec
  adc #0
  asl a
  asl a
  asl a
  sta cannon_missile_y,y
  lda cursor_x,x
  sec
  rol a
  asl a
  asl a
  sta cannon_missile_dstx,y
  lda cursor_y,x
  sec
  rol a
  asl a
  asl a
  sta cannon_missile_dsty,y
  
  ; Visually mark cannon as in cooldown
  lda player_cannon_y,x
  jsr seek_fielddata_row_a
  ldy player_cannon_x,x
  lda #$6F
  sta (fieldlo),y
  tya
  clc
  adc #32
  tay
  lda #$7F
  sta (fieldlo),y
  
  ldy player_cannon_x,x
  jsr update_around_column_y

  lda #SFX_LAUNCH
  jmp pently_start_sound
.endproc

;;
; Finds the index of the cannon closest to this player's
; (cursor_x, cursor_y).
; @param X player number (0 or 1)
.proc find_closest_cannon
best_distance = 0
best_index = 1
absdx = 2
absdy = 3
  lda #$FF
  sta best_distance
  sta best_index
  txa
  ora #(MAX_CANNONS - 1) << 1
  tay
cannonloop:

  ; Make sure this cannon is not busy cooling down
  lda cannon_cooldown_time,y
  bpl next_cannon

  ; Find the displacement to the target
  lda cannon_loc,y
  and #$F0
  lsr a
  lsr a
  lsr a
  sec
  sbc cursor_x,x
  bcs xispos
  eor #$FF
  adc #$01
xispos:
  sta absdx
  lda cannon_loc,y
  and #$0F
  asl a
  sec
  sbc cursor_y,x
  bcs yispos
  eor #$FF
  adc #$01
yispos:
  sta absdy
  
  ; Now approximate the distance with a metric that has an octagonal
  ; boundary: d = (dx + dy + max(dx, dy)) / 2
  cmp absdx
  bcs yismore
  lda absdx
yismore:
  ; At this point, A = max(dy, dx)
  clc
  adc absdy
  adc absdx

  ; Now A = max(dy, dx) + dx + dy which approximates twice the
  ; distance.  See if this twice distance is better than the
  ; previous twice distance.
  cmp best_distance
  bcs next_cannon
  sta best_distance
  sty best_index
next_cannon:
  dey
  dey
  bpl cannonloop
  ldy best_index
  lda best_distance
  rts
.endproc

;;
; Finds an unused slot for a cannon missile.
; A slot whose direction byte is negative is considered unused.
; @param X player number (0 or 1)
; @return Y = found slot ID or negative if none is free;
;         C = clear iff Y is valid
.proc find_cannon_missile_slot
  txa
  ora #(MAX_ROCKETS - 1) << 1
  tay
loop:
  lda cannon_missile_dir,y
  bmi got_one
  dey
  dey
  bpl loop
got_one:
  cpy #2 * MAX_ROCKETS
  rts
.endproc

;;
; A: subtenth number (3 or 4)
.proc cannon_cooldown
player = $06
cannonnum = $07
  and #$01
  sta player
  ora #(MAX_CANNONS - 1) << 1
  tax
cooldown_loop:
  lda cannon_loc,x
  cmp #$FF
  beq already_cool
  lda cannon_cooldown_time,x
  bmi already_cool
  dec cannon_cooldown_time,x
  bpl already_cool

  ; Visually mark cannon as no longer in cooldown
  stx cannonnum
  ldy cannonnum
  jsr get_cannon_y_pos
  lda 1
  jsr seek_fielddata_row_a
  lda #$6D
  sta (fieldlo),y
  tya
  clc
  adc #32
  tay
  lda #$7D
  sta (fieldlo),y

  ; And refresh tiles around cannon
  ldx player
  ldy 0
  jsr update_around_column_y
  lda #SFX_SCANBEEP
  jsr pently_start_sound
  ldx cannonnum
already_cool:
  dex
  dex
  bpl cooldown_loop
  
; Aim all of player's rockets
  txa
  and #$01
  ora #(MAX_ROCKETS - 1) << 1
  tax
rocketloop:
  lda cannon_missile_dir,x
  bmi no_rocket

  lda cannon_missile_x,x
  sta 0
  lda cannon_missile_y,x
  sta 1
  lda cannon_missile_dstx,x
  sta 2
  lda cannon_missile_dsty,x
  sta 3
  jsr getAngle
  sta cannon_missile_dir,x

no_rocket:
  dex
  dex
  bpl rocketloop
  rts
.endproc

.proc move_cannon_missiles
subx = 4
suby = 5
  ldx #2*MAX_ROCKETS-1
rocketloop:
  ldy cannon_missile_dir,x
  cpy #32
  bcc do_one_rocket
next_rocket:
  dex
  bpl rocketloop
  rts
do_one_rocket:
  ; Move in Y direction
  lda cannon_missile_subpx,x
  and #$0F
  clc
  adc missileSine,y
  sta suby
  lsr a
  lsr a
  lsr a
  lsr a
  eor #<-8
  clc
  adc #8
  clc
  adc cannon_missile_y,x
  sta cannon_missile_y,x

  lda cannon_missile_subpx,x
  lsr a
  lsr a
  lsr a
  lsr a
  clc
  adc missileCosine,y
  sta subx
  lsr a
  lsr a
  lsr a
  lsr a
  eor #<-8
  clc
  adc #8
  clc
  adc cannon_missile_x,x
  sta cannon_missile_x,x
  
  lda subx
  asl a
  asl a
  asl a
  asl a
  eor suby
  and #$F0
  eor suby
  sta cannon_missile_subpx,x
  
has_reached = $02
slop = 2

  ; when within a few px of target, snap to target and destroy
  lda #0
  sta has_reached
  lda cannon_missile_dstx,x
  sec
  sbc cannon_missile_x,x
  clc
  adc #slop
  cmp #2*slop+1
  bcs not_reached
  lda cannon_missile_dsty,x
  sec
  sbc cannon_missile_y,x
  clc
  adc #slop
  cmp #2*slop+1
  bcs not_reached
  ; the missile has reached its destination
  lda #$FF
  sta cannon_missile_dir,x
  sta has_reached
  lda cannon_missile_dsty,x
  sta cannon_missile_y,x
  lda cannon_missile_dstx,x
  sta cannon_missile_x,x

not_reached:
  lda cannon_missile_y,x
  lsr a
  lsr a
  lsr a
  sta 1
  lda cannon_missile_x,x
  lsr a
  lsr a
  lsr a
  sta 0
  
  ; If the missile has reached the destination, explode no matter
  ; what side it's on.  Otherwise explode only if on own side.
  lda has_reached
  bne destroy_either_side
  jsr check_missile_over_xy
  beq have_explode_carry
destroy_either_side:
  jsr check_missile_end_xy
have_explode_carry:
  bcs no_explode
  lda #$FF
  sta cannon_missile_dir,x
  txa
  pha
  jsr explode_missile
  pla
  tax
no_explode:
  jmp next_rocket
.endproc

;;
; Return carry clear iff the following are true:
; location is on opponent's territory,
; tile is destructible,
; and tile is tall enough to block missiles from passing over it
; and 2. the tile under the missile can be destroyed
; @param 0=X tile, 1=Y tile
.proc check_missile_over_xy
  lda 1
  lsr a
  tay  ; Y = road row number
  lda 0
  lsr a  ; A = road column number
  cmp xmax_1p,y  ; C = which side is it on?
  txa
  adc #$01  ; A.D0 = player number + side + 1
  and #$01
  lsr a     ; C = player number is same as side
  bcs nope
  jsr check_missile_end_xy
  bcs nope
  cmp #TILE_FLOWERS
  beq nope
yup:
  clc
nope:
  rts
.endproc

;;
; @param 0=X, 1=Y
; @return carry clear iff the tile here can be destroyed;
; A=tile number with bits 7-1 valid
.proc check_missile_end_xy

  ; The rocket has reached its destination, and the coords are in
  ; $00=X, $01=Y
  jsr tile_to_addr
  bcs nope  ; out of bounds
  lda (fieldlo),y
  cmp #TILE_FENCE
  beq yup
  and #$FE
  cmp #TILE_FLOWERS
  beq yup
  ; CONNBLKS_ROW plus 0-15: walls; plus 16-17: doors
  cmp #CONNBLKS_ROW
  bcc nope
  cmp #CONNBLKS_ROW+18
  rts
yup:
  clc
  .byte $B0  ; never-taken bcs to skip one byte
nope:
  sec
  rts
.endproc

;;
; Destroys a target tile and the four tiles around it
; @param Y column
; @param fieldlo pointer to start of row
; @param $0001 row number
.proc explode_missile
  ; Destroy this tile
  lda #TILE_RUBBLE
  sta (fieldlo),y

  ; Check splash to west
  dey
  jsr check_splash_here
  iny

  ; Check splash to east
  iny
  jsr check_splash_here
  dey

  ; Check splash to north  
  dec 1
  jsr tile_to_addr
  bcs no_n_splash
  jsr check_splash_here
no_n_splash:
  inc 1
  
  ; Check splash to south
  inc 1
  jsr tile_to_addr
  bcs no_s_splash
  jsr check_splash_here
no_s_splash:
  dec 1
  
  ; Choose which side to update if updating is busy
  cpy #16
  lda #0
  asl a
  tax
  jsr update_around_column_y
  lda #SFX_EXPLODE
  jmp pently_start_sound

;;
; Turns a cell vulnerable to splash damage into rubble
check_splash_here:
  lda (fieldlo),y
  cmp #TILE_FENCE
  beq is_splashable
  and #$FE
  cmp #TILE_FLOWERS
  beq is_splashable
  cmp #CONNBLKS_DOORWE
  bne not_splashable
  is_splashable:
    lda #TILE_RUBBLE
    sta (fieldlo),y
  not_splashable:
  rts
.endproc

; PRESENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Draws the cannon cursor for player X.  If the player has a
; cannon target position, draws three dots along the path from
; the target to the cursor.
; @param X player whose player_cannon_x and player_cannon_y to use
.proc draw_cannon_cursor
yend = OAM+0  ; returned from draw_player_x_cursor
xend = OAM+3
y1 = OAM+4    ; temporary cannon location, then position of
x1 = OAM+7    ; dot at 1/4 distance
y2 = OAM+8    ; position of dot at 1/2 distance
x2 = OAM+11
y3 = OAM+12   ; position of dot at 3/4 distance
x3 = OAM+15

  lda #TILE_PIECE_CURSOR
  jsr draw_player_x_cursor  ; places cursor coords in yend,y and xend,y
  lda player_cannon_x,x
  bmi skip_dots             ; skip if there is no cannon target

  ; Calculate actual cannon location (will not be displayed;
  ; 1/4 dot replaces it)
  asl a
  asl a
  asl a
  adc #FIELDTOP_X + 4
  sta x1,y
  lda player_cannon_y,x
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y + 4
  sta y1,y

  ; Place center dot
  adc yend,y
  ror a
  sta y2,y
  lda x1,y
  adc xend,y
  ror a
  sta x2,y

  ; Place 1/4 dot
  adc x1,y
  ror a
  sta x1,y
  lda y2,y
  adc y1,y
  ror a
  sta y1,y

  ; Place 3/4 dot
  lda x2,y
  adc xend,y
  ror a
  sta x3,y
  lda y2,y
  adc yend,y
  ror a
  sta y3,y
  
  lda #TILE_PATH_DOT
  sta OAM+5,y
  sta OAM+9,y
  sta OAM+13,y
  txa  ; player color
  sta OAM+6,y
  sta OAM+10,y
  sta OAM+14,y

  ; and include these in the display list
  tya
  clc
  adc #16
  sta oam_used
skip_dots:
  rts
.endproc

.proc draw_cannon_missiles
  ldx #2 * MAX_ROCKETS-1
loop:

  lda cannon_missile_dir,x
  bmi no_missile
  and #$1F
  lsr a
  lsr a
  lsr a
  tay  ; Y = quadrant number
  lda missileTileHotspotY,y
  sta 0
  lda missileTileFlip,y
  sta 2
  lda missileTileHotspotX,y
  sta 3
  lda cannon_missile_dir,x
  and #$0F
  tay
  lda missileTileNumber,y
  ldy oam_used
  sta OAM+1,y
  sec
  lda cannon_missile_y,x
  sbc 0
  clc
  adc #FIELDTOP_Y
  sta OAM,y
  lda 2
  sta OAM+2,y
  lda cannon_missile_x,x
  sec
  sbc 3
  clc
  adc #FIELDTOP_X
  sta OAM+3,y
  tya
  clc
  adc #4
  sta oam_used

no_missile:
  dex
  bpl loop
  rts
.endproc

.segment "RODATA"
missileTileNumber:
  .byt $27,$2F,$2E,$2D,$2C,$2B,$2A,$29,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F

; These are indexed by angle >> 3
missileTileFlip:
  .byt $00,$40,$C0,$80
missileTileHotspotY:
  .byt 6
missileTileHotspotX:  
  .byt 6, 1, 1, 6
