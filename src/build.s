;
; RHDE build phase
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

.segment "ZEROPAGE"
cur_piece_lo: .res 2
cur_piece_hi: .res 2
next_piece: .res 2

BUILD_PHASE_DURATION = 22  ; usually 22
PLACE_LAST_PIECE_DURATION = 4  ; usually 4
USE_4X4_PREVIEW = 1
SEQUENTIAL_PIECES = 0
.importzp NUM_PIECES

.segment "CODE"
.proc build_draw_bg
  ldy #0
  sty enclose_turn
  sty enclose_state
  sty enclose_play_sfx
  sty copy_region_left
  lda #30
  sta copy_region_width
  lda #DIRTY_ENCLOSE
  sta side_dirty
  sta side_dirty+1
  rts
.endproc

.proc draw_polyomino_cursor
base_x = 4
base_y = 5
piece_lo = 0

  ldx cur_turn
  lda cur_piece_hi,x
  cmp #$FF
  bne not_done
  rts
not_done:
  lda cursor_x,x
  sta base_x
  lda cursor_y,x
  sta base_y
  lda cur_piece_lo,x
  sta piece_lo
  lda cur_piece_hi,x
  lsr a
  ldy oam_used
  ldx #8
loop:
  bcc no_block
  
  ; carry is set, so compute base + offset + 1 to account for
  ; the 8 pixel border at the left side
  lda polyomino_offset_x,x
  adc base_x
  asl a
  asl a
  asl a
  sta OAM+3,y
  clc
  lda polyomino_offset_y,x
  adc base_y
  asl a
  asl a
  asl a
  clc
  adc #FIELDTOP_Y - 1
  sta OAM,y
  lda cur_turn
  sta OAM+2,y
  lda #TILE_PIECE_CURSOR
  cpx #8
  bne not_center_tile
  lda nmis
  lsr a
  bcc :+
  lda cur_turn
  ora #$40
  sta OAM+2,y
:
  lda #TILE_PIECE_CENTER
not_center_tile:
  sta OAM+1,y
  iny
  iny
  iny
  iny
no_block:
  asl piece_lo
  dex
  bpl loop

  ; Draw preview
  ldx cur_turn
  lda next_piece,x
  bmi no_preview
.if ::USE_4X4_PREVIEW = 0
  tax
  lda polyomino_preview_tiles,x
  and #$C0
  ora cur_turn
  sta OAM+2,y
  lda polyomino_preview_tiles,x
  and #$3F
  sta OAM+1,y
  lda base_y
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y - 1 - 16
  sta OAM,y
  lda base_x
  asl a
  asl a
  asl a
  adc #8
  sta OAM+3,y
  iny
  iny
  iny
  iny
.else
  ; FIXME! Larger preview
  tax  ; X = piece number
  lda base_y
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y - 1 - 14
  sta base_y
  lda base_x
  asl a
  asl a
  asl a
  adc #6
  sta base_x
preview_data_end = 1
  lda polyomino_preview_offset,x
  sta preview_data_end
  txa
  beq :+
  lda polyomino_preview_offset-1,x
:
  tax
prev4x4_blockloop:
  lda polyomino_preview_tiles,x
  and #%00001111
  ora #TILE_PREVIEW_BASE
  sta OAM+1,y
  lda polyomino_preview_tiles,x
  and #%00110000
  lsr a
  lsr a
  adc base_x
  sta OAM+3,y
  lda polyomino_preview_tiles,x
  and #%11000000
  lsr a
  lsr a
  lsr a
  lsr a
  eor #$FC
  adc base_y
  sta OAM+0,y
  lda cur_turn
  sta OAM+2,y  
  iny
  iny
  iny
  iny
  inx
  cpx preview_data_end
  bcc prev4x4_blockloop

.endif
no_preview:
  sty oam_used
  rts
.endproc

.proc get_next_piece
  ldx cur_turn
  lda next_piece,x
  
  ; piece ID > 128 means build phase time has expired
  ; and no more pieces will be dealt
  bpl not_end_of_pieces
  lda #$FF
  sta cur_piece_hi,x
  rts
not_end_of_pieces:
  asl a
  tay
  lda polyomino_shapes,y
  sta cur_piece_lo,x
  lda polyomino_shapes+1,y
  sta cur_piece_hi,x

  ; now ghetto piece generation
.if ::SEQUENTIAL_PIECES
  inc next_piece,x
  lda next_piece,x
  cmp #NUM_PIECES
  bcc :+
  lda #0
:
.else
  jsr lru4_deal_piece
.endif
  sta next_piece,x
  rts
.endproc

.proc rotate_piece
  ldx cur_turn
  lda cur_piece_hi,x
  bmi no_rotation
  lda cur_piece_lo,x
  lsr a
  bcc :+
  sbc #$80
:
  lsr a
  bcc :+
  sbc #$80
:
  sta cur_piece_lo,x
  lda #SFX_TURN
  jsr pently_start_sound
no_rotation:
  ; fall through to wall kick
.endproc

;;
; Moves the piece toward the player's region.
.proc wall_kick
xmin = 2
xmax = 3
base_x = 4
base_y = 5
piece_lo = 6
directions_pushed = 7
  lda #0
  sta directions_pushed

; First check against top and bottom walls
  ldx cur_turn
  lda cursor_y,x
  sta base_y
  lda cursor_x,x
  sta base_x
  
  ; Part 1: Top and bottom walls.  Do this as a separate loop because
  ; we can't compare the piece against the road until we know what
  ; part of the road we're on.
  lda cur_piece_lo,x
  sta piece_lo
  lda cur_piece_hi,x
  lsr a
  ldy #8
yloop:
  bcc yno_block
  clc
  lda polyomino_offset_y,y
  adc base_y
  bpl nonneg_y
  ; A is the Y coordinate of the out-of-bounds block.
  ; Move down by -A.
  sec
  eor #$FF
  adc base_y
  sta base_y
  lda #KEY_UP
  ora directions_pushed
  sta directions_pushed
  bne yno_block
nonneg_y:
  sec
  sbc #FIELD_HT - 1
  bcc yno_block
  eor #$FF
  adc base_y
  sta base_y
  lda #KEY_DOWN
  ora directions_pushed
  sta directions_pushed
yno_block:
  asl piece_lo
  dey
  bpl yloop

  ; Part 2: Border and road
  ; Now try horizontally, which is a bit trickier because of the
  ; road coordinate.
  lda cur_piece_lo,x
  sta piece_lo
  lda cur_piece_hi,x
  lsr a
  ldy #8
xloop:
  bcc xno_block
  ; Use the Y coordinate of this block to calculate the left and
  ; right bounds of the area in which the player may place blocks
  clc
  lda polyomino_offset_y,y
  adc base_y
  lsr a
  tax
  lda cur_turn
  bne is_2p
  lda xmax_1p,x
  asl a
  adc #$FF
  sta xmax
  lda #1
  sta xmin
  bne have_xmin_xmax
is_2p:
  lda xmin_2p,x
  asl a
  sta xmin
  lda #28
  sta xmax
have_xmin_xmax:
  clc
  lda polyomino_offset_x,y
  adc base_x
  bmi below_xmin
  cmp xmin
  bcs not_below_xmin
below_xmin:
  sec
  sbc xmin
  sec
  eor #$FF
  adc base_x
  sta base_x
  lda #KEY_LEFT
  ora directions_pushed
  sta directions_pushed
  bne xno_block
not_below_xmin:
  sbc xmax
  bcc xno_block
  eor #$FF
  adc base_x
  sta base_x
  lda #KEY_RIGHT
  ora directions_pushed
  sta directions_pushed
xno_block:
  asl piece_lo
  dey
  bpl xloop

  ; Now copy the freshly kicked coords back to the cursor
  ldx cur_turn
  lda base_y
  sta cursor_y,x
  lda base_x
  sta cursor_x,x
  lda directions_pushed
  rts
.endproc

;;
; Checks for blocks under the player's piece and adds a piece
; if they're not present.
.proc place_piece
piece_lo = 6
  ldx cur_turn
  jsr get_piece_top_left_corner
  lda cur_piece_hi,x
  lsr a
  lda cur_piece_lo,x
  sta piece_lo
  ldx #8
chkloop:
  bcc chknoblk
  lda polyomino_offset_y,x
  asl a
  asl a
  asl a
  asl a
  asl a
  clc
  adc polyomino_offset_x,x
  clc
  adc #33
  tay
  lda (fieldlo),y
  cmp #TILE_FLOOR
  beq chknoblk
  and #$FC
  cmp #TILE_GRASS
  beq chknoblk
  rts
chknoblk:
  asl piece_lo
  dex
  bpl chkloop

  ; Now place the blocks: use controlled block if placed within an
  ; enclosed (floor) area or stray block otherwise
  ldx cur_turn
  lda cur_piece_hi,x
  lsr a
  lda cur_piece_lo,x
  sta piece_lo
  ldx #8
putloop:
  bcc putnoblk
  lda polyomino_offset_y,x
  asl a
  asl a
  asl a
  asl a
  asl a
  clc
  adc polyomino_offset_x,x
  clc
  adc #33
  tay
  lda (fieldlo),y
  cmp #TILE_FLOOR
  bne not_floor
  lda #TILE_OV_BLOCK
  bne have_new_block
not_floor:
  lda #TILE_OV_STRAY_BLOCK
have_new_block:
  sta (fieldlo),y
putnoblk:
  asl piece_lo
  dex
  bpl putloop
  lda #0
  rts
.endproc

;;
; Loads the address of the cell one up and to the left of player
; X's cursor into the field adddress
; Modifies fieldlo, fieldhi, and A, but little else
.proc get_piece_top_left_corner
  lda cursor_y,x
  clc
  adc #<((fielddata >> 5) - 2)
  sta fieldhi
  lda #$F8
  lsr fieldhi
  ror a
  lsr fieldhi
  ror a
  lsr fieldhi
  ror a
  adc cursor_x,x
  sta fieldlo
  bcc :+
  inc fieldlo+1
:
  rts
.endproc


;;
; Does most of the work for the move phase.
.proc build_move_player
  ldx cur_turn
  lda das_keys,x
  and #KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT
  sta das_keys,x
  jsr autorepeat
  lda new_keys,x
  bpl no_A
  jsr place_piece
  bne nothing_placed
  jsr get_next_piece
  jsr update_around_cursor
  lda #DIRTY_ENCLOSE
  ora side_dirty,x
  sta side_dirty,x
  lda #$FF
  sta enclose_play_sfx
  lda #SFX_PLACE
  jmp pently_start_sound

nothing_placed:
  rts
no_A:
  and #KEY_B
  beq no_B
  jsr rotate_piece
no_B:
.endproc
.proc build_handle_shifting
  lda new_keys,x
  lsr a
  bcc not_right
  inc cursor_x,x
  jmp no_shifting
not_right:
  lsr a
  bcc not_left
  dec cursor_x,x
  bmi bad_left_shift
  bne no_shifting
bad_left_shift:
  lda #1
  sta cursor_x,x
  bne no_shifting
not_left:
  lsr a
  bcc not_down
  inc cursor_y,x
  jmp no_shifting
not_down:
  lsr a
  bcc not_up
  dec cursor_y,x
  bpl not_up
bad_up_shift:
  lda #0
  sta cursor_y,x
not_up:  
no_shifting:
  jmp wall_kick
.endproc

.proc build_phase
  ; While in forced blank we have full access to VRAM.
  ; Load the nametable (background map).
  jsr build_draw_bg
  
  ; Set up pieces
  ldx #1
player_init:
  stx cur_turn
  jsr lru4_deal_piece
  asl a
  tay
  lda polyomino_shapes,y
  sta cur_piece_lo,x
  lda polyomino_shapes+1,y
  sta cur_piece_hi,x
  jsr lru4_deal_piece
  sta next_piece,x
  lda #FIELD_HT / 2
  sta cursor_y,x
  dex
  bpl player_init

  lda #6
  sta cursor_x+0
  lda #23
  sta cursor_x+1
  lda #0
  sta time_tenths
  sta time_subtenths
  lda #BUILD_PHASE_DURATION
  sta phase_seconds
  
forever:
  jsr read_pads
  jsr pently_update

  lda cur_piece_hi+0
  and cur_piece_hi+1
  cmp #$FF
  beq end_phase

  jsr countdown_logic
  bcs not_54321
  bit next_piece+0
  bmi not_54321
  lda #SFX_COUNTDOWN
  jsr pently_start_sound
not_54321:
  lda phase_seconds
  bne not_new_second
  
  ; seconds has become zero.
  bit next_piece+0
  bpl kill_next
end_phase:
  lsr enclose_play_sfx
  rts
kill_next:
  lda #$FF
  sta next_piece+0
  sta next_piece+1
  lda #PLACE_LAST_PIECE_DURATION
  sta phase_seconds
;  jsr pently_stop_music
not_new_second:

  ldx #1
turns:
  stx cur_turn
  lda cur_piece_hi,x
  cmp #$FF
  beq :+
  jsr build_move_player
:
  ldx cur_turn
  dex
  bpl turns

  ; The most time consuming part is the enclosure state machine.
  jsr enclose_run_step

  ; We're not using sprite 0 for anything special here
  ldx #0
  stx oam_used
  ldx #1
  stx cur_turn
:
  jsr draw_polyomino_cursor
  dec cur_turn
  bpl :-
  jsr build_draw_timer
  
  ldx oam_used
  jsr ppu_clear_oam
  jsr build_choose_redraw
  jsr bgup_vsync
  jmp forever
.endproc

.proc build_draw_timer
  ldy oam_used
  lda phase_seconds
  jsr bcd8bit
  ldx #128
  jsr putdig
  lda 0
  beq no_tens
  ldx #120
  jsr putdig
no_tens:
  sty oam_used
  rts
putdig:
  sta OAM+1,y
  lda #FIELDTOP_Y + (FIELD_HT - 1) * 4 - 1
  sta OAM,y
  lda #0
  sta OAM+2,y
  txa
  sta OAM+3,y
  iny
  iny
  iny
  iny
  rts
.endproc

; Polyomino data structure
; 3 2 1
; 4 8 0
; 5 6 7
; Bit 15: Turn on to inhibit rotation (for I1, O4, X5)
; To rotate a piece, rotate bits 0-7 by 2 bits.
; Left: asl a adc #0 asl a adc #0
; Right: lsr a bcc :+ sbc #$80 : lsr a bcc :+ sbc #$80 :
; Then run wall kick again

.segment "RODATA"
polyomino_offset_x: .byt 1, 1, 0, <-1, <-1, <-1, 0, 1, 0
polyomino_offset_y: .byt 0, <-1, <-1, <-1, 0, 1, 1, 1, 0
polyomino_shapes:
  ; . . .
  ; . % .
  ; . . .
  .word $8100
  ; . . .
  ; . % %
  ; . . .
  .word $0101
  ; . . .
  ; % % %
  ; . . .
  .word $0111
  ; . % .
  ; . % %
  ; . . .
  .word $0105

  ; % . .
  ; % % %
  ; . . .
  .word $0119
  ; . . %
  ; % % %
  ; . . .
  .word $0113
  ; . % .
  ; % % %
  ; . . .
  .word $0115

  ; . % %
  ; % % .
  ; . . .
  .word $0116
  ; % % .
  ; . % %
  ; . . .
  .word $010D

  ; % . %
  ; % % %
  ; . . .
  .word $011B
  ; . % .
  ; % % %
  ; . % .
  .word $8155
  ; . % %
  ; . % .
  ; % % .
  .word $0166
  ; % % .
  ; . % .
  ; . % %
  .word $01CC

  ; % . .
  ; % . .
  ; % % %
  .word $00F8
  ; . % .
  ; . % .
  ; % % %
  .word $01E4
  ; % . .
  ; % % %
  ; . % .
  .word $0159
  ; . . %
  ; % % %
  ; . % .
  .word $0153

  ; % . .
  ; % % .
  ; . % %
  .word $01D8

.if USE_4X4_PREVIEW = 0
polyomino_preview_tiles:
  ; bit 6: set for h flipping
  ; bit 0-3: actual tile number to use
  .byt $10,$11,$12,$13      ; 1, i2, i3, l3
  .byt $15,$55,$14          ; j4, l4, t4
  .byt $16,$56              ; s4, z4
  .byt $17,$18,$19,$59      ; u, x, s5, z5
  .byt $1A,$1B,$1C,$5C,$1D  ; v5, t5, f, r, w
.else
.proc polyomino_preview_offset
  .byt p_2 - p_1
  .byt p_i3 - p_1
  .byt p_l3 - p_1
  .byt p_j4 - p_1
  .byt p_l4 - p_1
  .byt p_t4 - p_1
  .byt p_s4 - p_1
  .byt p_z4 - p_1
  .byt p_u5 - p_1
  .byt p_x5 - p_1
  .byt p_s5 - p_1
  .byt p_z5 - p_1
  .byt p_v5 - p_1
  .byt p_t5 - p_1
  .byt p_f5 - p_1
  .byt p_r5 - p_1
  .byt p_w5 - p_1
  .byt p_end - p_1

; Previews are made out of these seven tiles:
; . .   . .   % .   . %   % .   % .   . %
; % .   % %   % .   % .   . %   % %   % %
; 0     1     2     3     4     5     6
; Upper nibble is 0, 4, 8 for Y offset plus 0, 1, 2 for X offset
p_1:  .byt $10
p_2:  .byt $11
p_i3: .byt $00,$11
p_l3: .byt $15
p_j4: .byt $05,$20
p_l4: .byt $01,$22
p_t4: .byt $06,$20
p_s4: .byt $03,$13
p_z4: .byt $04,$14
p_u5: .byt $05,$22
p_x5: .byt $46,$13
p_s5: .byt $06,$91
p_z5: .byt $15,$81
p_v5: .byt $05,$20,$80
p_t5: .byt $06,$20,$90
p_f5: .byt $13,$45
p_r5: .byt $04,$56
p_w5: .byt $45,$11
p_end:
.endproc
polyomino_preview_tiles = polyomino_preview_offset::p_1
.endif

