.include "nes.h"
.include "ram.h"

.segment "BSS"
dropped_furni_x: .res MAX_DROPPED_FURNI
dropped_furni_y: .res MAX_DROPPED_FURNI  ; when negative, slot is unused
dropped_furni_type: .res MAX_DROPPED_FURNI

.segment "CODE"
.proc init_dropped_furni
  ldx #MAX_DROPPED_FURNI-1
  lda #$FF
loop:
  sta dropped_furni_y,x
  dex
  bpl loop
  rts
.endproc

;;
; Draws dropped furniture as sprites.
.proc draw_dropped_furni
  ldy #MAX_DROPPED_FURNI-1
  ldx oam_used
loop:
  lda dropped_furni_y,y
  bmi nofurni
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y
  sta OAM+0,x
  lda dropped_furni_x,y
  asl a
  asl a
  asl a
  adc #FIELDTOP_X
  sta OAM+3,x
  lda #TILE_DROPPED_FURNI
  sta OAM+1,x
  lda dropped_furni_type,y
  and #$01
  sta OAM+2,x
  inx
  inx
  inx
  inx
nofurni:
  dey
  bpl loop
  stx oam_used
  rts
.endproc

;;
; Add a dropped furni of type A at (X, Y).
; Trashes $00-$01
; @return carry set iff overflow
.proc add_dropped_furni
xsave = $00
typesave = $01
  stx xsave
  sta typesave
  ldx #MAX_DROPPED_FURNI-1
loop:
  lda dropped_furni_y,x
  bmi foundit
  dex
  bpl loop
  sec
  rts
  
foundit:
  tya
  sta dropped_furni_y,x
  lda typesave
  sta dropped_furni_type,x
  lda xsave
  sta dropped_furni_x,x
  clc
  rts
.endproc

;;
; @param (X, Y) tile on which to look for dropped furni
; @return X: index of dropped furni matching coordinates
; Z flag: true iff found
; 0: X, 1: Y
.proc query_dropped_furni
xsave = $00
ysave = $01
  stx xsave
  sty ysave
  ldx #MAX_DROPPED_FURNI - 1
loop:
  lda ysave
  cmp dropped_furni_y,x
  bne nonequal
  lda xsave
  cmp dropped_furni_x,x
  beq found
nonequal:
  dex
  bpl loop
found:
  rts
.endproc

;;
; Removes dropped furni at point X in list.
; @param X the entry in the dropped furni list
; @return A: the type of the removed dropped furni
.proc take_dropped_furni
  lda #$FF
  cpx #MAX_DROPPED_FURNI
  bcs invalid_x
  sta dropped_furni_y,x
  lda dropped_furni_type,x
invalid_x:
  rts
.endproc

;;
; Moves each dropped item to the inventory of the player on the
; same side of the road.
.proc cleanup_dropped_furni
  ldy #MAX_DROPPED_FURNI - 1
loop:
  lda dropped_furni_y,y
  bmi no_furni_here
  lsr a
  tax

  ; Find center line
  clc
  lda xmax_1p,x
  adc xmin_2p,x
  eor #$FF
  sec
  adc dropped_furni_x,y
  ; C = 0 left side, 1 for right side

  lda dropped_furni_type,y
  and #$FE  ; clear owner bit
  adc #0    ; and change it to C
  cmp #2 * NUM_FURNIS
  bcs no_furni_here
  tax
  inc player_inv,x

no_furni_here:
  dey
  bpl loop
  rts
.endproc

