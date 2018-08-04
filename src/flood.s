;
; SWAR CA flood fill for RHDE
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

;
; This module implements a cellular automaton that performs flood
; fill on a 16x24 cell area of the playfield.  It uses SWAR (single
; instruction multiple data within a register) processing on a
; bitmapped representation of the field.
; https://en.wikipedia.org/wiki/SWAR
;

.include "nes.inc"
.include "global.inc"

INCLUDE_DUMP_WALL = 1

.segment "BSS"
.align 128
fielddata:  .res FIELD_HT * 32
floodscratch:
walldata_l: .res FIELD_HT
walldata_r: .res FIELD_HT
seeddata_l: .res FIELD_HT
seeddata_r: .res FIELD_HT
xmax_1p: .res ROAD_HT
xmin_2p: .res ROAD_HT

side_dirty: .res 2
enclose_state: .res 1
enclose_turn: .res 1  ; 0: Evaluate left player 0 first; 1: Evaluate right player first
enclose_play_sfx: .res 1  ; Bit 7: Play sound effect when enclosed
unused_seedwt_lo: .res 2
unused_seedwt_hi: .res 1

.segment "CODE"

;;
; Converts one player's side of the field to a 1-bit bitmap of
; wall vs. not wall.
; ~14500 cycles
; @param X player (0: left; 1: right)
; @param C 0: treat connected doors as walls; 1: as walkable
.proc field_to_walldata
pf = fieldlo
pfhi = fieldhi
wall_tile_count = $02
player = $04
rownum = $05
xmin = $06
xmax = $07
  stx player

  lda #18
  bcc :+
  lda #16
:
  sta wall_tile_count

  lda #>fielddata
  sta pfhi
  txa
  beq :+
  lda #14
:
  sta pf
  ldx #0
  
  ; X holds vertical position and Y holds horizontal position
rowloop:
  ; Calculate minimum and maximum X and Y coords
  stx rownum
  txa
  lsr a
  tax
  lda xmin_2p,x
  asl a
  adc #<-14
  sta xmin
  lda xmax_1p,x
  asl a
  sta xmax
  lda player
  beq set_xmin_for_1p
  lda #16
  sta xmax
  bne have_xmin_xmax
set_xmin_for_1p:
  sta xmin
have_xmin_xmax:
  ldy xmax
  dey
  ldx rownum
  lda #$FF
  sta walldata_l,x
  sta walldata_r,x
tileloop:
  cpy xmin
  bcs not_to_left
  sec
  bcs have_carry
not_to_left:
  lda (pf),y
  
  ; Include walls and doors when disconnected
  eor #TILE_ENCLOSE_BASE
  cmp #TILE_ENCLOSE_COUNT
  bcc have_carry
  
  ; Include walls when connected
  eor #TILE_ENCLOSE_BASE^CONNBLKS_ROW
  cmp wall_tile_count
have_carry:
  ror walldata_l,x
  ror walldata_r,x
  dey
  bpl tileloop
  lda pf
  clc
  adc #32
  sta pf
  bcc :+
  inc pfhi
:
  inx
  cpx #FIELD_HT
  bcc rowloop
  rts
.endproc

;;
; Calculates an initial seed covering the area from the left and
; right sides of each row to the nearest wall.
; ~3600 cycles
.proc seed_sides
tmp1L = 0
tmp1R = 1
  lda #$FF
  sta seeddata_l+0
  sta seeddata_r+0
  sta seeddata_l+FIELD_HT-1
  sta seeddata_r+FIELD_HT-1
  ldy #FIELD_HT-2
loop:
  lda walldata_l,y
  cmp #$FF
  beq left_seed_is_full
  ; There's something in the left half.  This means the right half
  ; of the seed will be 0, and zeroes in the left half will be
  ; smeared right by 7.
  sec
  ror a
  and walldata_l,y
  sta tmp1L
  ror a
  ror a
  ora #$C0
  and tmp1L
  sta tmp1L
  ror a
  ror a
  ror a
  ror a
  ora #$F0
  and tmp1L
  ora #$80
  sta seeddata_l,y
  lda #$01
  bne have_left_seed_R
left_seed_is_full:
  sta seeddata_l,y
  lda walldata_r,y
  sec
  ror a
  and walldata_r,y
  sta tmp1R
  ror a
  ror a
  ora #$C0
  and tmp1R
  sta tmp1R
  ror a
  ror a
  ror a
  ror a
  ora #$F0
  and tmp1R
  ora #$01
have_left_seed_R:
  sta seeddata_r,y
  
  ; If the far right bit of walldata is on (no wall), then
  ; flood in from the right too.
  lda walldata_r,y
  lsr a
  bcc no_right_seed

  ; -x & (x + 1) finds the rightmost bit that forms a transition
  ; (0 to 1 or back)
  ; sec  ; carry set by previous BCC
  lda #0
  sbc walldata_r,y
  sta tmp1R
  lda #0
  sbc walldata_l,y
  sta tmp1L
  
  clc
  lda walldata_r,y
  adc #1
  and tmp1R
  sta tmp1R
  lda walldata_l,y
  adc #0
  and tmp1L
  sta tmp1L
  
  ; now subtract 1 to set all bits to the right of the chosen bit
  ; and combine with the left seed
  lda tmp1R
  bne :+
  dec tmp1L
:
  dec tmp1R
  lda tmp1R
  ora seeddata_r,y
  sta seeddata_r,y
  lda tmp1L
  ora seeddata_l,y
  sta seeddata_l,y
no_right_seed:
  dey
  beq done
  jmp loop
done:
  rts
.endproc

;;
; Removes any area known not to be enclosed from the seed, then
; adds the seed to the area known not to be enclosed.  Returns
; false (Z=1) if the seed is empty or true (Z=0) otherwise.
; ~1900 cycles
.proc clip_seed
found_any = 0
  ldy #0
  sty found_any
  ldy #(FIELD_HT * 2) - 1
loop:
  ; Remove already found areas from seed
  lda walldata_l,y
  and seeddata_l,y
  sta seeddata_l,y
  ora found_any
  sta found_any
  ; Mark seed as found
  lda seeddata_l,y
  eor #$FF
  and walldata_l,y
  sta walldata_l,y
  dey
  bpl loop
  lda found_any
  rts
.endproc

;;
; Copies the remaining enclosed area and its 8-neighborhood to seed.
; ~3600 cycles
.proc surround_floor
  ; First copy walldata to seeddata, then extend the seed once
  ; to include walls adjoining the enclosed area.
  ldy #FIELD_HT * 2 - 1
loop:
  lda walldata_l,y
  sta seeddata_l,y
  dey
  bpl loop

  ; fall through to extend_seed
.endproc

;;
; Extends the seed to its 8-neighborhood.
; ~3000 cycles
.proc extend_seed
  ldy #FIELD_HT - 1
extend_lr_loop:
  ; extend to left
  lda seeddata_r,y
  asl a
  ora seeddata_r,y
  sta seeddata_r,y
  lda seeddata_l,y
  rol a
  ora seeddata_l,y
  sta seeddata_l,y
  ; extend to right
  lsr a
  ora seeddata_l,y
  sta seeddata_l,y
  lda seeddata_r,y
  ror a
  ora seeddata_r,y
  sta seeddata_r,y  
  dey
  bpl extend_lr_loop
  ldy #FIELD_HT - 2
extend_next_row_loop:
  lda seeddata_l+1,y
  ora seeddata_l,y
  sta seeddata_l+1,y
  lda seeddata_r+1,y
  ora seeddata_r,y
  sta seeddata_r+1,y
  dey
  bpl extend_next_row_loop
  iny
extend_prev_row_loop:
  lda seeddata_l+1,y
  ora seeddata_l,y
  sta seeddata_l,y
  lda seeddata_r+1,y
  ora seeddata_r,y
  sta seeddata_r,y
  iny
  cpy #FIELD_HT - 1
  bcc extend_prev_row_loop
  rts
.endproc

;;
; Converts grass to walls where seeddata is true.
; ~13500 cycles
.proc seeddata_to_field_floor
pf = fieldlo
pfhi = fieldhi
mindirty = $02
maxdirty = $03
player = $04
rownum = $05
  stx player
  txa
  beq :+
  lda #14
:
  sta pf
  lda #>fielddata
  sta pfhi
  ldx #15
  stx mindirty
  ldx #0
  stx maxdirty
rowloop:
  ldy #15
tileloop:
  lsr seeddata_l,x
  ror seeddata_r,x
  bcc not_enclosed
  lda (pf),y
  cmp #TILE_OV_STRAY_BLOCK
  bne not_stray
  lda #TILE_OV_BLOCK
  bne have_new_tile
  bcs not_enclosed
not_stray:
  and #%11111100
  cmp #$04
  bne not_enclosed
  lda #TILE_FLOOR
have_new_tile:
  sta (pf),y
  cpy mindirty
  bcs :+
  sty mindirty
:
  cpy maxdirty
  bcc :+
  sty maxdirty
:
not_enclosed:
  dey
  bpl tileloop
  lda pf
  clc
  adc #32
  sta pf
  bcc :+
  inc pfhi
:
  inx
  cpx #FIELD_HT
  bcc rowloop
  
  ldx mindirty
  ldy maxdirty
  rts
.endproc

;;
; Given walldata (0=wall, 1=otherwise), put the wall tiles without at
; least two wall neighbors (1=zero or one neighbors, 0=2+ neighbors
; or not wall) into seeddata.
; About 6,300 cycles
.proc find_wall_ends

; 1: Does not have at least one neighbor
; 0: Has at least one neighbor
one_nbr_l = 0
one_nbr_r = 1

; 1: Does not have at least two neighbors
; 0: Has at least two neighbors
two_nbrs_l = 2
two_nbrs_r = 3

  ldx #FIELD_HT - 1
rowloop:
  lda #$FF
  sta two_nbrs_l
  sta two_nbrs_r
  
  ; Start with right neighbor
  sec
  lda walldata_l,x
  ror a
  sta one_nbr_l
  lda walldata_r,x
  ror a
  sta one_nbr_r
  
  ; Add left neighbor
  sec
  lda walldata_r,x
  rol a
  tay
  lda walldata_l,x
  rol a
  jsr add_ay_neighbors
  
  cpx #0
  beq do_bottom_neighbor
  lda walldata_l-1,x
  ldy walldata_r-1,x
  jsr add_ay_neighbors
skip_top_neighbor:

  cpx #FIELD_HT - 1
  bcs skip_bottom_neighbor
do_bottom_neighbor:
  lda walldata_l+1,x
  ldy walldata_r+1,x
  jsr add_ay_neighbors
skip_bottom_neighbor:

  lda walldata_l,x
  eor #$FF
  and two_nbrs_l
  sta seeddata_l,x
  lda walldata_r,x
  eor #$FF
  and two_nbrs_r
  sta seeddata_r,x
  dex
  bpl rowloop
  rts
  
; Adds the cells in common with one neighbor to two neighbors,
; then adds the cells to one neighbor.
; Row is A on left and Y on right.
add_ay_neighbors:
  pha
  ora one_nbr_l
  and two_nbrs_l
  sta two_nbrs_l
  pla
  and one_nbr_l
  sta one_nbr_l
  tya
  ora one_nbr_r
  and two_nbrs_r
  sta two_nbrs_r
  tya
  and one_nbr_r
  sta one_nbr_r
  rts
.endproc

;;
; Replaces tiles marked in the seed with the correct grass data and
; removes them from the walldata.
; About 10,000 cycles
.proc seeddata_to_grass
pf = fieldlo
pfhi = fieldhi
player = $04
rownum = $05
grasstile_xor = $06
  stx player
  txa
  beq :+
  lda #14
:
  sta pf
  sta grasstile_xor
  lda #>fielddata
  sta pfhi
  ldx #0
rowloop:
  ldy #15
  lda seeddata_l,x
  ora walldata_l,x
  sta walldata_l,x
  lda seeddata_r,x
  ora walldata_r,x
  sta walldata_r,x
tileloop:
  lsr seeddata_l,x
  ror seeddata_r,x
  bcc not_enclosed
  tya
  eor grasstile_xor
  and #$03
  ora #TILE_GRASS
  sta (pf),y
not_enclosed:
  dey
  bpl tileloop
  inx
  lda #$02
  eor grasstile_xor
  sta grasstile_xor
  lda pf
  clc
  adc #32
  sta pf
  bcc rowloop
  inc pfhi
  cpx #FIELD_HT
  bcc rowloop
  rts
.endproc

;;
; Converts floor to dark floor within the seed area and dark floor
; to floor outside.
; Faster than seeddata_to_field_floor
.proc seeddata_to_shaded_floor
pf = fieldlo
pfhi = fieldhi
player = $04
rownum = $05
xmin = $06
xmax = $07
  stx player
  txa
  beq :+
  lda #14
:
  sta pf
  lda #>fielddata
  sta pfhi
  ldx #0
rowloop:
  stx rownum
  txa
  lsr a
  tax
  lda xmin_2p,x
  asl a
  adc #<-14
  sta xmin
  lda xmax_1p,x
  asl a
  sta xmax
  lda player
  beq set_xmin_for_1p
  lda #16
  sta xmax
  bne have_xmin_xmax
set_xmin_for_1p:
  sta xmin
have_xmin_xmax:
  ldy #15
  ldx rownum
tileloop:
  lsr seeddata_l,x
  ror seeddata_r,x
  lda (pf),y
  bcc not_enclosed
  ; carry set: doorless area
  cmp #TILE_FLOOR
  bne skip
  beq flip
not_enclosed:
  cmp #TILE_DARK_FLOOR
  bne skip
flip:
  cpy xmin
  bcc skip
  cpy xmax
  bcs skip
  eor #TILE_FLOOR^TILE_DARK_FLOOR
  sta (pf),y
skip:
  dey
  bpl tileloop
  lda pf
  clc
  adc #32
  sta pf
  bcc :+
  inc pfhi
:
  inx
  cpx #FIELD_HT
  bcc rowloop
  rts
.endproc

; DEBUG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.if INCLUDE_DUMP_WALL
.proc dump_wall_seed
  lda #$20
  sta PPUADDR
  asl a
  sta PPUADDR
  ldy #$00
loop:
  lda walldata_l,y
  jsr dump8
  lda walldata_r,y
  jsr dump8
  lda seeddata_l,y
  jsr dump8
  lda seeddata_r,y
  jsr dump8
  iny
  cpy #FIELD_HT
  bcc loop
  rts
  
dump8:
  sec
  rol a
johnson:
  ldx #$10
  bcc :+
  ldx #0
:
  stx PPUDATA
  asl a
  bne johnson
  rts
.endproc

.proc interactive_encloop
  jsr enclose_run_step
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  jsr dump_wall_seed
waitloop:
  lda nmis
:
  cmp nmis
  beq :-  
  ldx #0
  ldy #0
  lda #VBLANK_NMI
  clc
  jsr ppu_screen_on
  jsr read_pads
  lda new_keys
  bpl waitloop
  lda side_dirty+0
  ora side_dirty+1
  and #(DIRTY_DOORLESS|DIRTY_RM_ENDS|DIRTY_ENCLOSE)
  ora enclose_state
  bne interactive_encloop
  rts
.endproc


.endif


; THE STATE MACHINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ENCLOSE_STEP_SEARCH = 0
ENCLOSE_STEP_MAKE_SEED = 2
ENCLOSE_STEP_CONTINUE = 4
ENCLOSE_STEP_MARK = 6
ENCLOSE_STEP_RM_ENDS = 8
ENCLOSE_STEP_DOORLESS_SEARCH = 10
ENCLOSE_STEP_DOORLESS_MAKE_SEED = 12
ENCLOSE_STEP_DOORLESS_CONTINUE = 14
ENCLOSE_STEP_DOORLESS_MARK = 16

.proc enclose_run_step
  ldx enclose_state
  lda enclose_steps+1,x
  pha
  lda enclose_steps,x
  pha
  rts
.pushseg
.segment "RODATA"
enclose_steps:
  .addr enclose_search-1, enclose_make_seed-1
  .addr enclose_continue-1, enclose_mark-1
  .addr enclose_rm_ends-1
  .addr enclose_search-1, enclose_make_seed-1
  .addr enclose_continue-1, enclose_mark-1
.popseg
.endproc

.proc enclose_search
  ; Find player whose dirty flags include needing an enclose
  ldx enclose_turn
  lda side_dirty,x
  and #DIRTY_ENCLOSE|DIRTY_RM_ENDS|DIRTY_DOORLESS
  bne found_something
  ; If not, check other player
  txa
  eor #1
  tax
  lda side_dirty,x
  and #DIRTY_ENCLOSE|DIRTY_RM_ENDS|DIRTY_DOORLESS
  bne found_something
  rts  
found_something:
  ; acknowledge enclose in progress
  ldy #ENCLOSE_STEP_MAKE_SEED
  and #DIRTY_DOORLESS|DIRTY_RM_ENDS
  beq have_step
  cmp #DIRTY_DOORLESS
  bcc is_rm_ends
  ldy #ENCLOSE_STEP_DOORLESS_MAKE_SEED
  lda #<~DIRTY_DOORLESS
  bne have_step_and_mask
is_rm_ends:
  ldy #ENCLOSE_STEP_RM_ENDS
have_step:
  lda #<~(DIRTY_RM_ENDS|DIRTY_ENCLOSE|DIRTY_DOORLESS)
have_step_and_mask:
  and side_dirty,x
  sta side_dirty,x

  ; and start it
  sty enclose_state
  stx enclose_turn
  cpy #ENCLOSE_STEP_DOORLESS_MAKE_SEED
  jmp field_to_walldata
.endproc

.proc enclose_rm_ends
  jsr find_wall_ends
  ldx #ENCLOSE_STEP_MAKE_SEED
  stx enclose_state
  ldx enclose_turn
  lda #DIRTY_SIDE
  ora side_dirty,x
  sta side_dirty,x
  jmp seeddata_to_grass
.endproc

.proc enclose_make_seed
  jsr seed_sides
  jsr clip_seed
  jsr extend_seed
  inc enclose_state
  inc enclose_state
  jmp clip_seed
  rts
.endproc

.proc enclose_continue
  jsr extend_seed
  jsr clip_seed
  beq done_filling
  jsr extend_seed
  jmp clip_seed
done_filling:
  inc enclose_state
  inc enclose_state
  jmp surround_floor
.endproc

.proc enclose_mark
  ldx enclose_turn
  lda enclose_state
  cmp #ENCLOSE_STEP_DOORLESS_MARK
  bne not_doorless
  jsr seeddata_to_shaded_floor
  jmp set_recopy
not_doorless:
  jsr seeddata_to_field_floor
  lda 3
  cmp 2
  bcc nothing_done

  bit enclose_play_sfx
  bpl set_recopy
  lda #SFX_ENCLOSE
  jsr start_sound
set_recopy:
  ldx enclose_turn
  lda side_dirty,x  ; Mark this side as needing recopied
  ora #DIRTY_SIDE
  sta side_dirty,x

nothing_done:
  lda enclose_turn  ; Give the other side priority next time
  eor #1
  sta enclose_turn

  lda #0
  sta enclose_state
  rts
.endproc

