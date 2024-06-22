;
; RHDE alert boxes between phases
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

.segment "BSS"
short_string_buf: .res 16

.segment "CODE"

;;
; Puts a 144x64 tile window on the screen.
.proc draw_alertbox_frame
dstlo = 0
dsthi = 1
rowsleft = 2
  ldx #VBLANK_NMI
  stx PPUCTRL
  lda #$00
  sta PPUMASK
  lda #$3F
  sta PPUADDR
  ldy #$1D
  sty PPUADDR
  sta PPUDATA
  sta PPUDATA
  lda #$38
  sta PPUDATA

  ; Top row
  lda #$21
  sta PPUADDR
  sta dsthi
  lda #$66
  sta dstlo
  lda #$47
  sta PPUADDR
  lda #TILE_FRAME_TOP
  jsr same16
rowloop:
  lda dsthi
  sta PPUADDR
  lda dstlo
  sta PPUADDR
  clc
  adc #32
  sta dstlo
  bcc :+
  inc dsthi
:
  lda #TILE_FRAME_LEFT
  sta PPUDATA
  ldy #8
tileloop:
  stx PPUDATA
  inx
  stx PPUDATA
  inx
  dey
  bne tileloop
  lda #TILE_FRAME_RIGHT
  sta PPUDATA
  cpx #$E0
  bcc rowloop

  lda #$22
  sta PPUADDR
  lda #$27
  sta PPUADDR
  lda #TILE_FRAME_BOTTOM
same16:
  ldy #8
:
  sta PPUDATA
  sta PPUDATA
  dey
  bne :-
  rts
.endproc

.proc load_alertbox_oam
  ldx oam_used
  ldy #0
loop:
  lda cornersdata,y
  sta OAM,x
  iny
  inx
  cpy #16
  bcc loop
  sty oam_used
  rts
.pushseg
.segment "RODATA"
cornersdata:
  .byt  83, TILE_FURNI_HALO, $03, 56
  .byt  83, TILE_FURNI_HALO, $43, 192
  .byt 139, TILE_FURNI_HALO, $83, 56
  .byt 139, TILE_FURNI_HALO, $C3, 192
.popseg
.endproc

.proc prepare_alertbox
  sta ciSrc+1
  sty ciSrc
  ldx #$08
clr_vwf_loop:
  stx cursor_x
  lda nmis
:
  cmp nmis
  beq :-
  ldy #32
  jsr clear_vwf_rows
  jsr screen_back_on
  jsr pently_update
  ldx cursor_x
  inx
  inx
  cpx #$0E
  bcc clr_vwf_loop

  lda #0
  sta cursor_y

lineloop:
  jsr draw_alertbox_line
  lda nmis
:
  cmp nmis
  beq :-
  lda cursor_y+1
  ora #$08
  ldy #$00
  jsr copyLineImg
  jsr screen_back_on
  jsr pently_update
  lda cursor_y
  cmp #6
  bcc lineloop

  lda #0
  sta oam_used
  jsr load_alertbox_oam
  ldx oam_used
  jsr ppu_clear_oam
  lda nmis
:
  cmp nmis
  beq :-
  lda #>OAM
  sta OAM_DMA

  ; Grass color changes take effect when an alertbox is being shown
  ldy #$3F
  sty PPUADDR
  lda #$02
  sta PPUADDR
  lda player_grass_color+0
  sta PPUDATA
  sty PPUADDR
  lda #$06
  sta PPUADDR
  lda player_grass_color+1
  sta PPUDATA

  jsr draw_alertbox_frame
  jsr screen_back_on
  
  ; Don't try to redraw the playfield on top of this
  ldx #0
  stx copy_region_width
  inx
:
  lda #<~DIRTY_SIDE
  and side_dirty,x
  sta side_dirty,x
  dex
  bpl :-

  jsr pently_update
  jmp read_pads
.endproc

.proc draw_alertbox_line
str_width = 2
str_left = 3
scores_lo = 12
scores_hi = 14

  jsr clearLineImg
  lda cursor_y+0
  sta cursor_y+1
loop_y0:
  ldy #0
loop:
  lda (ciSrc),y
  iny
  cmp #$20
  bcs greaterthan0x20
  cmp #$10
  bcs greaterthan0x10
  ; $00-$0F: Set line number
  sta cursor_y+0
  cmp cursor_y+1
  beq loop
  jsr fixup_ciSrc
  lda #16
  jmp invertTiles
greaterthan0x10:
  ; $10-$1F: Left aligned text
  and #$0F
  asl a
  asl a
  asl a
  tax
  jsr get_text_addr
left_str_0_at_x:
  jsr vwfPuts0
  jmp loop_y0
greaterthan0x20:
  cmp #$40
  bcs greaterthan0x40
  cmp #$30
  bcs greaterthan0x30
  ; $21-$2F: Centered text
  and #$0F
  asl a
  asl a
  asl a
  sta 3  ; 3: x base
  jsr get_text_addr
center_str_0_at_3:
  jsr vwfStrWidth0  ; A, 2: str width

  ; If centering would push it past the left side, clip to left
  lsr a
  cmp 3
  bcc @not_off_left
  ldx #0
  beq left_str_0_at_x
@not_off_left:
  sec
  eor #$FF
  adc 3
  tax
  adc 2
  ; If centering would push it past the right side, clip to right
  bpl left_str_0_at_x
  lda #128
  sec
  sbc 2
  tax
  jmp left_str_0_at_x

greaterthan0x30:
  ; 30-3F: Right aligned text
  adc #<-$2F
  asl a
  asl a
  asl a
  sta 3
  jsr get_text_addr
right_str_0_at_3:
  jsr vwfStrWidth0
  eor #$FF
  sec
  adc 3
  bcs :+
  lda #0
:
  tax
  jmp left_str_0_at_x

  ; FIXME
  jmp loop
greaterthan0x40:
  cmp #$50
  bcs greaterthan0x50
  jsr get_scores_dispatch
  lda scores_hi+1
  ldy scores_lo+1
  ldx #96
  jsr right_align_digits  
  lda scores_hi
  ldy scores_lo
  ldx #64
  jsr right_align_digits  
  jmp loop_y0

get_text_addr:
  ; Get an address into $00 and fixup ciSrc
  lda (ciSrc),y
  iny
  sta 0
  lda (ciSrc),y
  iny
  sta 1
fixup_ciSrc:
  clc
  tya
  adc ciSrc
  sta ciSrc
  bcc :+
  inc ciSrc+1
:
  rts

greaterthan0x50:
  ; $50: Player wins
  jsr fixup_ciSrc
  lda unitzp24+3
  cmp unitzp24+2
  bne have_winner_in_c
  lda unitzp24+1
  cmp unitzp24+0
  bne have_winner_in_c
  ; Drawn game
  ldx #64
  stx 3
  lda #>furnishresult_draw
  sta 1
  lda #<furnishresult_draw
  sta 0
  jmp center_str_0_at_3
have_winner_in_c:
  ldx #0
  bcc @have_x
  ldx #2
@have_x:
  lda winner_msgs,x
  sta 0
  lda winner_msgs+1,x
  sta 1
  bcs :+
  ldx #8
  jmp left_str_0_at_x
:
  lda #120
  sta 3
  jmp right_str_0_at_3

get_scores_dispatch:
  and #$0F
  asl a
  tax
  lda get_scores_table+1,x
  pha
  lda get_scores_table,x
  pha
  jmp fixup_ciSrc
.pushseg
.segment "RODATA"
get_scores_table:
  .addr get_scores_cash-1, get_scores_piece-1, get_scores_xy-1
  .addr get_scores_hra-1
.popseg

get_scores_cash:  ; $40
  ldx #3
:
  lda cash_lo,x
  sta scores_lo,x
  dex
  bpl :-
  rts

get_scores_piece:  ; $41
  ldx #3
:
  lda cur_piece_lo,x
  sta scores_lo,x
  dex
  bpl :-
  rts

get_scores_xy:  ; $42
  ldx #3
:
  lda unitzp24,x
  sta scores_lo,x
  dex
  bpl :-
  rts

get_scores_hra:  ; $43
  ldx #1
:
  lda #0
  sta scores_hi,x
  lda hra_wins,x
  sta scores_lo,x
  dex
  bpl :-
  rts
.endproc

;;
; Writes 5-digit number AAYY at position X.
; @return Y: number of digits written (1-5)
.proc right_align_digits
hidig = $00
ndigits = $01
penx = $0B
charsleft = $0A

  sta bcdNum+1
  sty bcdNum
  stx 11
  jsr bcdConvert
  lda #$00
  sta hidig
  sta ndigits
  ldx #4
toascii_loop:
  lda bcdResult,x
  beq :+
  lda #$30
  sta hidig
  lda bcdResult,x
:
  ora hidig
  sta short_string_buf,x
  dex
  bne toascii_loop
  lda bcdResult+0
  ora #$30
  sta short_string_buf+0
  ldy #4
write_loop:
  sty charsleft
  lda penx
  tax
  clc
  adc #5
  sta penx
  lda short_string_buf,y
  beq :+
  inc ndigits
  jsr vwfPutTile
:
  ldy charsleft
  dey
  bpl write_loop
  ldy ndigits
  rts
.endproc

.proc remove_alertbox
  lda #7
  jsr pently_start_sound
  jsr delay_500
  lda #6
  sta copy_region_left
  lda #18
  sta copy_region_width
  ldx #0
  stx b_hold_time
  jmp ppu_clear_oam
.endproc

.segment "RODATA"
; $00-$05: Flush current line and start line Y
END_OF_ALERT = $0F
; $10-$3F: align and print a string
ALIGN_CENTER = $28
ALIGN_LEFT = $11
ALIGN_FARLEFT = $10
ALIGN_RIGHT = $3E
ALIGN_FARRIGHT = $3F
; $40-$4F: print pairs of 16-bit values retrieved through
; get_scores_table
SCORES_CASH = $40
SCORES_CUR_PIECE = $41
SCORES_UNIT_X = $42
SCORES_HRA = $43
WHO_WINS = $50

alert_furnish:
  .byte $00, ALIGN_CENTER
  .addr furnish_msg0
  .byte $02, ALIGN_CENTER
  .addr furnish_msg2
  .byte $03, ALIGN_CENTER
  .addr furnish_msg3
  .byte END_OF_ALERT

alert_furnish_result:
  .byte $00, WHO_WINS
  .byte $02, SCORES_UNIT_X, ALIGN_LEFT
  .addr furnishresult_msg2
  .byte $03, SCORES_HRA, ALIGN_LEFT
  .addr furnishresult_msg3
  .byte END_OF_ALERT

alert_battle:
  .byte $00, ALIGN_CENTER
  .addr battle_msg0
  .byte $02, ALIGN_CENTER
  .addr battle_msg2
  .byte $03, ALIGN_CENTER
  .addr battle_msg3
  .byte END_OF_ALERT

alert_build:
  .byte $00, ALIGN_CENTER
  .addr build_msg0
  .byte $02, ALIGN_CENTER
  .addr build_msg2
  .byte $03, ALIGN_CENTER
  .addr build_msg3
  .byte $05, ALIGN_CENTER
  .addr build_msg5
  .byte END_OF_ALERT

alert_doors:
  .byte $00, ALIGN_CENTER
  .addr doors_msg0
  .byte $02, ALIGN_CENTER
  .addr doors_msg2
  .byte $03, ALIGN_CENTER
  .addr doors_msg3
  .byte $04, ALIGN_CENTER
  .addr doors_msg4
  .byte END_OF_ALERT

alert_rent_result:
  .byte $00, ALIGN_CENTER
  .addr result_msg0
  .byte $02, SCORES_CUR_PIECE, ALIGN_LEFT
  .addr rentresult_msg2
  .byte $03, SCORES_UNIT_X, ALIGN_LEFT
  .addr rentresult_msg3
  .byte $04, SCORES_CASH, ALIGN_LEFT
  .addr rentresult_msg4
  .byte END_OF_ALERT

winner_msgs:
  .addr furnishresult_p1wins, furnishresult_p2wins

build_msg0: .byte "[ BUILD ]",0
build_msg2: .byte "Patch up walls with blocks",0
build_msg3: .byte "and expand your house.",0
build_msg5: .byte "+ Move  B: Turn  A: Place",0
doors_msg0: .byte "[ MAKE DOORS ]",0
doors_msg2: .byte "Add doors to your house",0
doors_msg3: .byte "to make it easier to get",0
doors_msg4: .byte "in and out.",0
furnish_msg0: .byte "[ FURNISH ]",0
furnish_msg2: .byte "Buy furniture and",0
furnish_msg3: .byte "add it to your house.",0
battle_msg0: .byte "[ BATTLE ]",0
battle_msg2: .byte "Make your house look better",0
battle_msg3: .byte "by vandalizing the other one.",0
result_msg0: .byte "[ RESULT ]",0
furnishresult_p1wins: .byt "< PLAYER 1 WINS",0
furnishresult_p2wins: .byt "PLAYER 2 WINS >",0
furnishresult_draw: .byt "DRAW!",0
furnishresult_msg2: .byt "Rating",0
furnishresult_msg3: .byt "Rounds won",0
rentresult_msg2: .byt "Area",0
rentresult_msg3: .byt "Earned",0
rentresult_msg4: .byt "Total", 0

