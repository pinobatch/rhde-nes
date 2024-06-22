;
; RHDE door creation and destruction
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

DOORS_PHASE_DURATION = 10
MAX_BREAKDOORS = 16  ; number of doors whose damage is tracked
DOOR_STRENGTH = 3    ; number of hits a door takes

.segment "BSS"
breakdoors_y: .res MAX_BREAKDOORS  ; bits 7-5: number of hits applied
breakdoors_x: .res MAX_BREAKDOORS

; Creation of doors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CODE"
.proc doors_phase
  lda #DOORS_PHASE_DURATION
  sta phase_seconds
  lda #0
  sta time_tenths
  sta time_subtenths
  sta cur_piece_lo+0
  sta cur_piece_lo+1
  lda #FIELD_HT / 2
  sta cursor_y+0
  sta cursor_y+1
  lda #6
  sta cursor_x+0
  lda #23
  sta cursor_x+1

loop:
  jsr read_pads
  jsr pently_update
  jsr move_cursors
  jsr enclose_run_step
  jsr countdown_logic
  bcs not_54321
  lda #SFX_COUNTDOWN
  jsr pently_start_sound
not_54321:
  lda phase_seconds
  bne not_last_second
  rts
not_last_second:

  ldx #0
  stx oam_used
  jsr build_draw_timer
  jsr draw_cursors
  ldx oam_used
  jsr ppu_clear_oam
  jsr build_choose_redraw
  jsr bgup_vsync
  jmp loop
  rts
.endproc

.proc move_cursors
  ldx #1
playerloop:
  stx cur_turn
  jsr move_one
  jsr eval_cursor
  lda new_keys,x
  bpl notA
  lda cur_piece_lo,x
  bmi notA
  beq notA
  cmp #TILE_GRASS
  bne notgrass
  jsr unlink_neighboring_tiles
  ldx cur_turn
  lda #DIRTY_ENCLOSE
  ora side_dirty,x
  sta side_dirty,x
  lda cursor_y,x
  asl a
  eor cursor_x,x
  and #$03
  ora #TILE_GRASS  
notgrass:
  ldy #33
  sta (fieldlo),y
  jsr update_around_cursor
  lda #DIRTY_DOORLESS
  ora side_dirty,x
  sta side_dirty,x
  lda #SFX_PLACE
  jsr pently_start_sound
  ldx cur_turn
notA:
  dex
  bpl playerloop

  ; Keep 1P on left of road
  lda cursor_y+0
  lsr a
  tay
  lda cursor_x+0
  lsr a
  cmp xmax_1p,y
  bcc :+
  dec cursor_x+0
:

  ; Keep 2P on right of road
  lda cursor_y+1
  lsr a
  tay
  lda cursor_x+1
  lsr a
  cmp xmin_2p,y
  bcs :+
  inc cursor_x+1
:
  rts
move_one:
  jsr autorepeat
  lda das_keys,x
  and #KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT
  sta das_keys,x
  lda new_keys,x
  lsr a
  bcc notRight
  lda cursor_x,x
  cmp #28
  bcs :+
  inc cursor_x,x
:
  rts
notRight:

  lsr a
  bcc notLeft
  lda cursor_x,x
  cmp #2
  bcc :+
  dec cursor_x,x
:
  rts
notLeft:

  lsr a
  bcc notDown
  lda cursor_y,x
  cmp #FIELD_HT-1
  bcs :+
  inc cursor_y,x
:
  rts
notDown:

  lsr a
  bcc notUp
  lda cursor_y,x
  beq :+
  dec cursor_y,x
:
  rts
notUp:

  rts
.endproc

;;
; See if the space under the cursor is door-eligible
; @return $01-$7F: valid door tile number; other: ineligible
.proc eval_cursor
  lda cur_piece_lo,x
  bpl :+
  rts
:
  ; --  1 --
  ; 32 33 34
  ; -- 65 --
  jsr get_piece_top_left_corner
  ldy #33
  lda (fieldlo),y
  eor #CONNBLKS_ROW
  cmp #16
  bcs ineligible
  
  ; Figure out what direction this wall is pointed in:
  cmp #CONNBLKS_E^CONNBLKS_W
  bne not_we

  ; WE doors cannot be on the N or S edge of the playfield
  lda cursor_y,x
  beq ineligible
  cmp #FIELD_HT - 1
  bcs ineligible

  ; West and east: Spaces to north and south must be passable,
  ; and spaces to west and east must be wall-type
  ldy #1
  lda (fieldlo),y
  jsr is_tile_passable
  bne ineligible
  ldy #65
  lda (fieldlo),y
  jsr is_tile_passable
  bne ineligible
  ldy #32
  lda (fieldlo),y
  and #$F0
  cmp #CONNBLKS_ROW
  bne ineligible
  ldy #34
  eor (fieldlo),y
  and #$F0
  bne ineligible
  lda #CONNBLKS_DOORWE
  bne have_door_tile
  
not_we:
  cmp #CONNBLKS_N^CONNBLKS_S
  bne not_ns

  ; NS doors cannot be on the W or E edge of the playfield
  lda cursor_x,x
  cmp #2
  bcc ineligible
  cmp #28
  bcs ineligible

  ; North and south: Spaces to west and east must be passable,
  ; and spaces to north and south must be wall-type
  ldy #32
  lda (fieldlo),y
  jsr is_tile_passable
  bne ineligible
  ldy #34
  lda (fieldlo),y
  jsr is_tile_passable
  bne ineligible
  ldy #1
  lda (fieldlo),y
  and #$F0
  cmp #CONNBLKS_ROW
  bne ineligible
  ldy #65
  eor (fieldlo),y
  and #$F0
  beq is_door_ns
ineligible:
  lda #0
  beq have_door_tile
is_door_ns:
  lda #CONNBLKS_DOORNS
  bne have_door_tile
  
not_ns:
wallbits = 0
walloffset = 1

  tay
  lda door_eligibility_table,y
  beq ineligible

  ; Check the corners that must be filled with a wall
  and #$F0
  sta wallbits
  lda #0
  sta walloffset
wallbitsloop:
  ldy walloffset
  inc walloffset
  asl wallbits
  bcc :+
  lda cornerwalloffsets,y
  tay
  lda (fieldlo),y
  eor #CONNBLKS_ROW
  cmp #16
  bcs ineligible
  lda wallbits
:
  bne wallbitsloop
  lda #TILE_GRASS
have_door_tile:
  sta cur_piece_lo,x
  rts
.endproc

.pushseg
.segment "RODATA"
door_eligibility_table:
;        +--------- Removal requires NW is wall
;        |+-------- Removal requires NE is wall
;        ||+------- Removal requires SW is wall
;        |||+------ Removal requires SE is wall
;        ||||+----- Eligible for removal by self
;        |||||
;        |||||
;        |||||
  .byte %00001000  ; lone block
  .byte %00001000  ; e
  .byte %00000000  ; we
  .byte %00001000  ; w
  .byte %00001000  ; s
  .byte %00010000  ; se
  .byte %00110000  ; swe
  .byte %00100000  ; sw
  .byte %00000000  ; ns
  .byte %01010000  ; nse
  .byte %11110000  ; nswe
  .byte %10100000  ; nsw
  .byte %00001000  ; n
  .byte %01000000  ; ne
  .byte %11000000  ; nwe
  .byte %10000000  ; nw
cornerwalloffsets:
  .byte 0, 2, 64, 66
.popseg

;;
; @param fieldlo pointer to the tile up and to the left
.proc unlink_neighboring_tiles
linkbits = 0

  ; The tile link data forms a Karnaugh map.
  ; 0100dcba
  ;     |||+- linked right xor left
  ;     ||+-- linked left
  ;     |+--- linked down xor up
  ;     +---- linked up
  ; So first we need to transform the coordinates from Karnaugh map
  ; coordinates to a bit for each direction
  ldy #33
  lda (fieldlo),y
  and #%00001010
  lsr a
  eor (fieldlo),y
  sta linkbits
  ldx #3
bitloop:
  lsr linkbits
  bcc notLinked
  ldy udlr_offsets,x
  lda (fieldlo),y
  and #$F0
  cmp #CONNBLKS_ROW
  bne notLinked
  lda udlr_flipvals,x
  eor (fieldlo),y
  sta (fieldlo),y
notLinked:
  dex
  bpl bitloop
  rts

.pushseg
.segment "RODATA"
udlr_offsets: .byte 1, 65, 32, 34
udlr_flipvals: .byte CONNBLKS_S, CONNBLKS_N, CONNBLKS_E, CONNBLKS_W
.popseg
.endproc

.proc draw_cursors
  ldx #1
  jsr draw_one
  dex
draw_one:
  lda cur_piece_lo,x
  bpl piece_exists
  rts
piece_exists:
  cmp #1
  lda #TILE_PATH_DOT
  bcc :+
    lda #TILE_PIECE_CURSOR
  :
  ; fall through
.endproc
;;
; Draws a tile at a player's cursor (cursor_x, cursor_y).
; @param X player (0 or 1)
; @param A tile number
; @return Y: offset in shadow OAM of the cursor
.proc draw_player_x_cursor
  ldy oam_used
  sta OAM+1,y  ; set tile
  txa
  sta OAM+2,y  ; set color
  lda cursor_y,x
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y-1
  sta OAM+0,y
  lda cursor_x,x
  asl a
  asl a
  asl a
  adc #8
  sta OAM+3,y
  tya
  clc
  adc #4
  sta oam_used
  rts
.endproc

; Destruction of doors and fences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc init_breakdoors
  ldx #MAX_BREAKDOORS - 1
  lda #0
loop:
  sta breakdoors_y,x
  dex
  bpl loop
  rts
.endproc

;;
; Add one point of damage to the door at tile (X, Y).
; @return carry set iff the door should be broken
.proc inc_breakdoors
xtile = 0
ytile = 1
blank_break = 2
  stx xtile
  sty ytile
  ldx #MAX_BREAKDOORS - 1  ; X: entry that we're checking
  ldy #$FF  ; Y: an unused entry (or UCHAR_MAX for none)
findloop:
  lda breakdoors_y,x
  cmp #FIELD_HT
  bcs notblank
  txa
  tay
notblank:
  and #$1F
  cmp ytile
  bne nothere
  lda breakdoors_x,x
  cmp xtile
  beq ishere
nothere:
  dex
  bpl findloop

  ; At this point, the door hasn't been hit at all.  Try to
  ; allocate an unused entry.
  cpy #FIELD_HT
  bcs no_space_for_door
  lda ytile
  ora #$20
  sta breakdoors_y,y
  lda xtile
  sta breakdoors_x,y
no_space_for_door:
  clc
  rts

ishere:
  lda breakdoors_y,x
  clc
  adc #$20
  cmp #DOOR_STRENGTH << 5
  bcc not_broken_yet
  lda #0
not_broken_yet:
  sta breakdoors_y,x
  rts
.endproc
