;
; RHDE race selection
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"
.include "mbyt.inc"
.segment "CODE"

.proc load_racesel_tiles
  lda #$80
  sta PPUCTRL
  asl a
  sta PPUMASK
  sta PPUADDR
  sta PPUADDR
  lda #>rsel_chr_pb53
  ldy #<rsel_chr_pb53
  ldx #144/4
  jsr load_blk_chr
  lda #$20
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #>rsel_nam_pb53
  ldy #<rsel_nam_pb53
  ldx #1024/64
  jsr load_blk_chr
  lda #$10
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #>rsel_sprites_pb53
  ldy #<rsel_sprites_pb53
  ldx #64*16/256
  jsr load_blk_chr
  lda #$3F
  ldx #$01
  sta PPUADDR
  stx PPUADDR
palloop:
  lda rsel_pal,x
  sta PPUDATA
  inx
  cpx #32
  bcc palloop

  ldx #$0E
  ldy #32
  jsr clear_vwf_rows

  ; Load race names
  jsr clearLineImg
  lda #>race_name0
  ldy #<race_name0
  ldx #2
  jsr vwfPuts
  lda #>race_name1
  ldy #<race_name1
  ldx #41
  jsr vwfPuts
  lda #>race_name2
  ldy #<race_name2
  ldx #70
  jsr vwfPuts
  lda #16
  jsr invertTiles
  lda #$0F
  ldy #$00
  jsr copyLineImg

  ; Load page title
  jsr clearLineImg
  lda #>racesel_title
  ldy #<racesel_title
  ldx #32
  jsr vwfPuts
  lda #16
  jsr invertTiles
  lda #$0E
  ldy #$00
  jsr copyLineImg
  lda #$0E
  ldy #$08
  jsr copyLineImg
  
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$20
  sta PPUADDR
  lda #$A8
  sta PPUADDR
  ldy #$E0
  ldx #16
titleblksloop:
  sty PPUDATA
  iny
  dex
  bne titleblksloop
  ; fall through
.endproc

.proc racesel_update_name
  lda #$22
  sta PPUADDR
  txa
  beq :+
  lda #20^9
:
  eor #$C9
  sta PPUADDR
  lda player_race,x
  asl a
  asl a
  ora #$F0
  tax
  ldy #$03
:
  stx PPUDATA
  inx
  dey
  bpl :-
  rts
.endproc

.proc racesel
  jsr load_racesel_tiles
mainloop:
  jsr read_pads
  ldx #0
  stx oam_used
  inx
playerloop:
  lda new_keys,x
  lsr a
  bcc notRight
  lda player_race,x
  cmp #2
  bcs keysDone
  inc player_race,x
  bne keysDone
notRight:
  lsr a
  bcc notLeft
  lda player_race,x
  beq keysDone
  dec player_race,x
  bpl keysDone
notLeft:
keysDone:
  dex
  bpl playerloop

  jsr racesel_draw_cursors
  ldx oam_used
  jsr ppu_clear_oam
  lda nmis
:
  cmp nmis
  beq :-
  ldx #1
  jsr racesel_update_name
  ldx #0
  jsr racesel_update_name
  ldx #0
  ldy #0
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sec
  jsr ppu_screen_on
  lda new_keys+0
  and #KEY_START
  beq mainloop
  rts
.endproc

.proc racesel_draw_cursors
  ldy oam_used
  ldx #1
player_loop:
  txa
  lda player_race,x
  lsr a
  ror a
  ror a
  adc racemarker_x,x
  sta OAM+3,y
  sta OAM+11,y
  adc #8
  sta OAM+7,y
  sta OAM+15,y
  lda racemarker_tile,x
  sta OAM+1,y
  eor #$01
  sta OAM+5,y
  eor #$09
  sta OAM+9,y
  eor #$01
  sta OAM+13,y
  lda #159
  sta OAM+0,y
  sta OAM+4,y
  lda #167
  sta OAM+8,y
  sta OAM+12,y
  lda nmis
  lsr a
  txa
  sta OAM+2,y
  sta OAM+6,y
  sta OAM+10,y
  sta OAM+14,y

  ; draw colored right border of race name
  ora #$80  ; Hflip; Vflip is in carry
  ror a     ; Add Vflip based on NMIs; player number down to carry
  adc #0    ; bring player number up to bit 0
  sta OAM+18,y
  lda #175
  sta OAM+16,y
  lda racename_rborder_x,x
  sta OAM+19,y
  lda #$04
  sta OAM+17,y
  tya
  clc
  adc #20
  tay
  dex
  bpl player_loop
  sty oam_used
  rts
.endproc

.segment "RODATA"
rsel_chr_pb53:
  .incbin "obj/nes/rsel.chr.donut"
rsel_nam_pb53:
  .incbin "obj/nes/rsel.nam.donut"
rsel_pal:
  .incbin "obj/nes/rsel.sav", $1F00, 16
  mbyt "FFFF1620 FFFF1220 FFFFFFFF FFFFFFFF"
rsel_sprites_pb53:
  .incbin "obj/nes/racesel_sprites.chr.donut"

racesel_title: .byte "[ Select Race ]", 0
race_name0: .byte "Nander",0
race_name1: .byte "Poli",0
race_name2: .byte "Volci",0

;racename_lborder_x: .byte 56, 56+88
racename_rborder_x: .byte 104, 104+88
racemarker_x: .byte 48, 64
racemarker_tile: .byte $00, $02
