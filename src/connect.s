;
; RHDE wall connection and disconnection
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "ram.h"

.segment "ZEROPAGE"
connect_progress: .res 1

.segment "CODE"
;;
; Connects wall blocks in the field into walls.
; Call start once, then call continue until carry is set.
.proc connect_field_start
  lda #>fielddata
  sta connect_progress
.endproc
.proc connect_field_continue
pf = 0
pfhi = 1
prevpf = 2
prevpfhi = 3
  lda connect_progress
  sta pfhi
  cmp #>fielddata
  bcs :+
  lda #0
:
  sec
  sbc #1
  sta prevpfhi
  lda #$E0
  sta prevpf
  lda #0
  sta pf

rowloop:
  ldy #28
tileloop:
  lda (pf),y
  eor #CONNBLKS_ROW
  cmp #16
  bcs not_already_conn_wall
  lda #CONNBLKS_ROW
  bne have_pf_blk
not_already_conn_wall:
  lda (pf),y
  eor #TILE_ENCLOSE_BASE
  cmp #TILE_ENCLOSE_COUNT
  bcc is_translated_wall

next_tile:
  dey
  bne tileloop
  lda pf
  sta prevpf
  clc
  adc #32
  sta pf
  lda pfhi
  sta prevpfhi
  bcc rowloop
  adc #0
  sta connect_progress
  cmp #>(fielddata + FIELD_HT * 32)
  rts

; Put the common case (the loop) first and have the uncommon case
; later.  This lets the common case use short (<=126 byte) branches.

is_translated_wall:
  tax
  lda door_translation,x
have_pf_blk:
  sta (pf),y

  ; now check the (already translated) block to the right
  iny
  lda (pf),y
  dey
  eor #CONNBLKS_ROW
  cmp #18  ; 16 walls 2 doors
  bcs not_r_conn
  cmp #16
  bcs :+
  iny
  eor #CONNBLKS_ROW^CONNBLKS_W
  sta (pf),y
  dey
:
  lda (pf),y
  eor #CONNBLKS_ROW
  cmp #16
  bcs not_r_conn
  eor #CONNBLKS_ROW^CONNBLKS_E
  sta (pf),y
not_r_conn:

  bit prevpfhi
  bmi not_u_conn
  lda (prevpf),y
  eor #CONNBLKS_ROW
  cmp #18  ; 16 walls 2 doors
  bcs not_u_conn
  cmp #16
  bcs :+
  eor #CONNBLKS_ROW|CONNBLKS_S
  sta (prevpf),y
:
  lda (pf),y
  eor #CONNBLKS_ROW
  cmp #16
  bcs not_u_conn
  eor #CONNBLKS_ROW|CONNBLKS_N
  sta (pf),y
not_u_conn:

  jmp next_tile

.pushseg
.segment "RODATA"
door_translation:
  .byt CONNBLKS_ROW, CONNBLKS_ROW, CONNBLKS_DOORWE, CONNBLKS_DOORNS
.popseg
.endproc

;;
; Converts connected walls to disconnected wall blocks and corrects
; grass. Call start once, then call continue until carry is set.
.proc disconnect_field_start
  lda #>fielddata
  sta connect_progress
.endproc
.proc disconnect_field_continue
pf = 0
pfhi = 1
grassxor = 7
  lda #<fielddata
  sta pf
  lda connect_progress
  sta pfhi
  lda #TILE_GRASS
  sta grassxor
rowloop:
  ldy #28
tileloop:
  lda (pf),y

  ; 2013-12-28: Turn all floor tiles into grass tiles in case a
  ; hole was blown in the wall
  cmp #$6F
  beq is_cannon
  cmp #$7F
  bne not_cannon
is_cannon:
  and #$FD
  bne have_new_tile
not_cannon:
  cmp #TILE_DARK_FLOOR
  beq is_grass
  cmp #TILE_FLOOR
  beq is_grass
  cmp #TILE_RUBBLE
  beq is_grass
  eor #TILE_GRASS
  ; Correct all grass tiles
  cmp #4
  bcs not_grass
is_grass:
  tya
  and #$03
  eor grassxor
  bne have_new_tile
not_grass:
  ; A = t ^ TILE_GRASS
  eor #TILE_GRASS^CONNBLKS_DOORWE
  ; A = t ^ CONNBLKS_DOORWE
  cmp #2
  bcs not_door
  ora #TILE_OV_DOOR_WE
  bcc have_new_tile
not_door:
  ; A = t ^ CONNBLKS_DOORWE
  eor #CONNBLKS_DOORWE^CONNBLKS_ROW
  cmp #16
  bcs not_exterior_wall
  lda #TILE_OV_STRAY_BLOCK
have_new_tile:
  sta (pf),y
not_exterior_wall:
no_change:
  dey
  bne tileloop
  lda grassxor
  eor #2
  sta grassxor
  lda pf
  clc
  adc #32
  sta pf
  bcc rowloop
  lda pfhi
  adc #0
  sta connect_progress
  cmp #>(fielddata + FIELD_HT * 32)
  rts
.endproc

;;
; Picks up outdoor furni that are indoors and indoor furni that are
; outdoors.
; Requires that the field be disconnected, as flood fill flows
; through connected doors.
.proc pickup_outdoor_furni
  lda #0
  sta cursor_y
  sta enclose_state
  sta side_dirty+0
  sta side_dirty+1
  ldx cur_turn
  stx enclose_turn
  lda #DIRTY_ENCLOSE
  sta side_dirty,x
  lda sidebase,x
  sta cursor_x
  jmp enclose_run_step
.endproc

.proc pickup_outdoor_furni_continue
  lda enclose_state
  beq enclose_state_ok
  jsr enclose_run_step
  clc
  rts
enclose_state_ok:

is_indoor_cell = $08  ; $00-$7F: indoor, $80-$FF: outdoor
road_x = $09
  ; Load the current row and its boundary
  lda cursor_y
  jsr seek_fielddata_row_a
  lda cursor_y
  lsr a
  tax
  lda xmax_1p,x
  asl a
  sta road_x


cell_loop:
  ; Make sure cursor_x is still in bounds
  lda cursor_x
  ldx cur_turn
  cmp sideend,x
  bcs next_row

  ; Get the inside/outside evaluation for this cell
  ldx cursor_y
  asl walldata_r,x
  rol walldata_l,x
  ror is_indoor_cell
  
  ; Compare the position of this cell on the left or right side
  ; of the road to the player number, to award furniture on one
  ; side of the road to the correct player
  ldy cursor_x
  cpy road_x     ; C = 0 for left side or 1 for right side
  rol a
  eor cur_turn
  lsr a          ; C = 0 for matching side of road or 1 for mismatch
  bcs next_cell  ; expected: bcs; inverse (debug): bcc

  ; Tiles $00-$13 and $40-$53 are never furni.  This short-circuits
  ; most of the work for the most common tiles: $40-$51 (connected
  ; walls and doors), $10-$13 (overhead walls and doors), and $00-$07
  ; (grass and flooring).
  lda (fieldlo),y
  and #$3F
  cmp #$14
  bcc next_cell

  ; Find which furni this is
  ldx #0
  lda (fieldlo),y
  jsr find_furni_tile_a
  bmi next_cell        ; not furni?
  ora #$00             ; not top left tile?
  bne next_cell
  cpy #NUM_ALL_FURNIS  ; valid furni?
  bcs next_cell

  ; At this point, furni ID is in Y, cursor is on the top left
  ; corner of the furni, and itemdatalo is correct
itemdata = $00
itemnum = $06
  sty itemnum
  ldy #SHOPITEMS_SIZE
  lda is_indoor_cell
  ror a
  ror a
  eor (itemdata),y  ; 7: tall; 6: wide; 5: indoor
  and #$20
  beq next_cell     ; don't pick up if cell matches furni
  eor (itemdata),y  ; load furni footprint with opposite in/outdoor
  ldx #0            ; use player 1's cursor to generate grass tiles
  jsr erase_furni

  ; Add the canonical version of this item to the player's inventory
  lda itemnum
  jsr find_furni_main_rot
  asl a
  ora cur_turn
  tax
  inc player_inv,x
  inc cursor_x
  rts

next_cell:
  inc cursor_x
  jmp cell_loop

next_row:
  lda sidebase,x
  sta cursor_x
  inc cursor_y
  lda cursor_y
  cmp #FIELD_HT
  ; C = 0 for not done or 1 for done
  rts
.endproc

.pushseg
.segment "RODATA"
sidebase: .byte 0, 14
sideend: .byte 16, 29
.popseg

