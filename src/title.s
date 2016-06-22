.include "nes.h"
.include "ram.h"
.include "mbyt.h"

; Uncomment to play music during the help screen.  Useful for
; testing a transcription that you just added to musicseq.s.
;MUSIC_TEST = 0
;SFX_TEST = SFX_SCANBEEP

.segment "ZEROPAGE"
rounds_to_win: .res 1

.segment "CODE"

NUM_PAGES = 5
.segment "CODE"

.proc title_screen
  ; Draw the main pattern table
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  sta PPUADDR
  sta PPUADDR
  lda #>title1_chr_pb53
  ldy #<title1_chr_pb53
  ldx #(128*128 + 64*8)/512
  jsr load_pb53_blk

  ; Draw the nametable
  lda #$20
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #>title1_nam_pb53
  ldy #<title1_nam_pb53
  ldx #1024/128
  jsr load_pb53_blk
  ldx #$24
  jsr ppu_zero_nt

  ; load text window
  lda #$00
  ldy #$A0
  jsr draw_half_of_text_window
  lda #$10
  ldy #$D0
  jsr draw_half_of_text_window
  ldx #$18
  jsr ppu_zero_nt
  ldx #$1C
  jsr ppu_zero_nt

  lda #$3F
  sta PPUADDR
  ldx #$01
  stx PPUADDR
loadtitlepalloop:
  lda titlepal,x
  sta PPUDATA
  inx
  cpx #$14
  bcc loadtitlepalloop
  lda #182-1
  sta OAM+0
  ldy #$01
  sty OAM+1
  lda #%10100000
  sta OAM+2
  sta OAM+3
  ldx #4
  jsr ppu_clear_oam

  ; load push start and copyright notices
  ; these are last because they change VRAM write direction
  jsr clearLineImg
  lda #>title_pushstart_msg
  ldy #<title_pushstart_msg
  ldx #16
  jsr vwfPuts
  lda #$1C
  ldy #$08
  jsr copyLineImg
  lda #$1C
  ldy #$00
  jsr copyLineImg
  jsr clearLineImg
  lda #>title_copr_msg
  ldy #<title_copr_msg
  ldx #16
  jsr vwfPuts
  lda #$1F
  ldy #$00
  jsr copyLineImg
  lda #$FF
  sta cursor_y
  
press_start_loop:
  jsr title_keywait
  and #KEY_START|KEY_A
  beq press_start_loop

  ; Clear out $1C00-$1FFF to make room for menu
  lda #$1C
  sta cursor_y
clear1cloop:
  lda nmis
:
  cmp nmis
  beq :-
  bit PPUSTATUS
  lda #$80
  sta PPUCTRL
  lda cursor_y
  sta PPUADDR
  lda #0
  sta PPUADDR
  sta PPUMASK
  ldx #64
:
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  dex
  bne :-
  tay
  lda #VBLANK_NMI|BG_0000
  clc
  jsr ppu_screen_on
  jsr update_sound
  inc cursor_y
  lda cursor_y
  cmp #$20
  bcc clear1cloop

  ; Load the menu items
  lda #0
  sta cursor_y
load_title_options_loop:
  jsr clearLineImg
  lda cursor_y
  asl a
  tax
  ldy title_options,x
  lda title_options+1,x
  ldx #16
  jsr vwfPuts

  lda nmis
:
  cmp nmis
  beq :-
  clc
  lda cursor_y
  adc #$1A
  ldy #0
  jsr copyLineImg
  ldx #0
  ldy #0
  lda #VBLANK_NMI|BG_0000
  jsr ppu_screen_on
  jsr update_sound
  inc cursor_y
  lda cursor_y
  cmp #NUM_TITLE_OPTIONS
  bcc load_title_options_loop

  ; Display arrow sprite and let the player move the cursor
  ; to select a mode
  ldy #0
  sty cursor_y
modesel_loop:
  jsr title_keywait
  and #KEY_A|KEY_START
  bne modesel_done
  lda new_keys
  and #KEY_UP
  beq modesel_notUp
  dec cursor_y
  bpl modesel_loop
  lda #NUM_TITLE_OPTIONS - 1
  sta cursor_y
  jmp modesel_loop
modesel_notUp:
  lda new_keys
  and #KEY_DOWN|KEY_SELECT
  beq modesel_notDown
  inc cursor_y
  lda cursor_y
  cmp #NUM_TITLE_OPTIONS
  bcc modesel_loop
  lda #0
  sta cursor_y
  beq modesel_loop
modesel_notDown:
  lda new_keys
  and #KEY_LEFT
  beq modesel_notLeft
  lda cursor_y
  sec
  sbc #3
  bcs :+
  lda #0
:
  sta cursor_y
  jmp modesel_loop
modesel_notLeft:
  lda new_keys
  and #KEY_RIGHT
  beq modesel_loop
  lda cursor_y
  clc
  adc #3
  cmp #NUM_TITLE_OPTIONS
  bcc :+
  lda #NUM_TITLE_OPTIONS - 1
:
  sta cursor_y
  jmp modesel_loop
  
  
modesel_done:
  
  lda cursor_y
  cmp #3
  beq do_uctions
  tax
  lda rtw_table,x
  sta rounds_to_win
  rts

do_uctions:
  ; now display the instructions
  ldx #0
uctionsloop:
  txa
  pha
  ldy uctions_pages,x
  lda uctions_pages+1,x
  ldx #KEY_A|KEY_START|KEY_SELECT
  jsr display_txt_file
  pla
  tax
  lda #KEY_START
  and new_keys+0
  bne end_instructions
  inx
  inx
  cpx #NUM_PAGES * 2
  bcc uctionsloop
end_instructions:
  jmp title_screen
.endproc

.proc title_keywait
  ldx #8
  lda cursor_y
  cmp #3
  bcc :+
  sbc #3
  ldx #136
:
  cmp #3
  bcc :+
  lda #8
:
  asl a
  asl a
  asl a
  adc #191
  sta OAM+4
  stx OAM+7
  lda #2
  sta OAM+5
  lda #0
  sta OAM+6

loop:
  lda nmis
:
  cmp nmis
  beq :-
  ldy #0
  ldx #0
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  lda #VBLANK_NMI|BG_0000|OBJ_1000|0
  sec
  jsr ppu_screen_on
  jsr update_sound
  jsr read_pads
s0w0:
  bit PPUSTATUS
  bvs s0w0
  lda #$C0
s0w1:
  bit PPUSTATUS
  beq s0w1
  bmi nos0
  lda #VBLANK_NMI|BG_0000|OBJ_1000|1
  sta PPUCTRL
  jsr waste36dots
  lda #VBLANK_NMI|BG_1000|OBJ_1000|1
  sta PPUCTRL
nos0:
  lda new_keys+0
  beq loop
waste36dots:
  rts
.endproc

;;
; @param A high byte of nul-terminated text to display
; @param Y low byte
; @param X bitmask of which player 1 keys will close it
.proc display_txt_file
txtaddrhi = cur_piece_hi
txtaddrlo = cur_piece_lo
exitkeys = next_piece
planedsthi = cursor_y
planedstlo = cursor_x
bytes_used = cur_piece_lo+1
chrplane = enclose_turn
ntdst = ciDst

  sty txtaddrlo
  sta txtaddrhi
  stx exitkeys
  lda #VBLANK_NMI
  sta PPUCTRL
  ldx #$00
  stx PPUMASK
  lda #$3F
  sta PPUADDR
  stx PPUADDR
palloop:
  lda titleguy_palette,x
  sta PPUDATA
  inx
  cpx #16
  bcc palloop

  ldx #$20
  jsr ppu_zero_nt
  
  ; Draw the character
  lda #$00
  sta PPUADDR
  sta PPUADDR
  lda #>titleguy_pb53
  ldy #<titleguy_pb53
  ldx #64*80/512
  jsr load_pb53_blk
  lda #$20
  sta 1
  lda #$E3
  sta 0
  ldy #0
  ldx #8
  lda #10
  jsr draw_1d_rect_to_bg

  ; Load the attributes for the text side
  lda #$23
  sta PPUADDR
  lda #$C8
  sta PPUADDR
  ldx #0
attrloop:
  lda #$00
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  lda #%11111010
  and #%11001100  ; left half zero, right half xx
  sta PPUDATA
  lda #%11111010
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  inx
  cpx #6
  bcc attrloop

  ; Start writing the text itself
  lda #$06
  sta planedsthi+1
  sta planedsthi+0
  lda #$08
  sta planedstlo+1
  lda #$00
  sta planedstlo+0
  lda #$20
  sta ntdst+1
  lda #$8E
  sta ntdst+0

load_todo_loop:
  jsr clearLineImg
  ldy txtaddrlo
  lda txtaddrhi
  ldx #0
  jsr vwfPuts
  sty txtaddrlo
  sta txtaddrhi

  ; Save the number of bytes used
  txa
  clc
  adc #7
  and #<-8
  sta bytes_used
  beq nowrite

  ; Copy tiles to the pattern table
  lda ntdst
  ldx #0
  and #$40
  beq :+
  inx
:
  stx chrplane

  lda planedsthi,x
  ldy planedstlo,x
  jsr copyLineImg
  lda #VBLANK_NMI
  sta PPUCTRL

  ; Write used tiles to the nametable
  lda bytes_used
  lsr a
  lsr a
  lsr a
  tax
  ldy chrplane
  lda planedsthi,y
  lsr a
  ora planedstlo,y
  ror a
  ror a
  ror a
  ror a
  tay
  lda ntdst+1
  sta 1
  lda ntdst
  sta 0
  lda #1
  jsr draw_1d_rect_to_bg

  ; Move data pointer to next tile
  ldx chrplane
  lda bytes_used
  asl a
  bcc :+
  inc planedsthi,x
  clc
:
  adc planedstlo,x
  bcc :+
  inc planedsthi,x
:
  sta planedstlo,x

nowrite:

  lda ntdst
  clc
  adc #32
  sta ntdst
  bcc :+
  inc ntdst+1
:
  lda ntdst+1  ; stop after $2380
  cmp #$23
  bcc :+
  lda ntdst+0
  bmi load_todo_done
:
  jmp load_todo_loop
load_todo_done:

.ifdef MUSIC_TEST
  lda #MUSIC_TEST
  jsr init_music
.endif

.ifdef SFX_TEST
  lda #SFX_TEST
  jsr start_sound
.endif

loop:
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
  jsr update_sound
  lda nmis
  sta CRCLO
  eor #$FF
  sta CRCHI
  lda new_keys
  and exitkeys
  beq loop
  rts
.endproc

;;
; @param Y starting tile number
; @param X width
; @param A height
; @param $00-$01 destination in nametable
.proc draw_1d_rect_to_bg
dstlo = 0
dsthi = 1
rows_left = 2
width = 3
  sta rows_left
  stx width
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
  ldx width
tileloop:
  sty PPUDATA
  iny
  dex
  bne tileloop
  dec rows_left
  bne rowloop
  rts
.endproc

;;
; Draws a 16x3-tile text window to the second nametable.
; @param A low byte of starting address (high byte fixed at $27)
; @param Y starting tile number
.proc draw_half_of_text_window
  sta 0
  lda #$27
  sta 1
  ldx #16
  lda #3
  jmp draw_1d_rect_to_bg
.endproc

.segment "RODATA"
.import __RODATA_LOAD__, __RODATA_SIZE__, __CODE_LOAD__, __CODE_SIZE__
ROMSIZE = __CODE_SIZE__ + __RODATA_SIZE__ + 6
ROMPCT = (1000 * ROMSIZE + 16384) / 32768
; started this project on Sun 2013-08-11
BUILDDAY = (.TIME / 86400) - 15928

titleguy_palette:
  mbyt "20FF3716 FF161616 FFFF20FF FF20FFFF"
titleguy_pb53:
  ; 64x80 pixel character
  .incbin "obj/nes/titleguy.chr.pb53"
uctions_pages:
  .addr uctions1_txt, uctions2_txt, uctions3_txt, uctions4_txt, uctions5_txt
uctions1_txt:
  .incbin "uctions1.txt"
  .byte 0
uctions2_txt:
  .incbin "uctions2.txt"
  .byte 0
uctions3_txt:
  .incbin "uctions3.txt"
  .byte 0
uctions4_txt:
  .incbin "uctions4.txt"
  .byte 0
uctions5_txt:
  .incbin "uctions5.txt"
  .byte LF, LF, .sprintf("Day %d: ROM ", BUILDDAY)
  .byte '0'|<(ROMPCT / 100 .MOD 10)
  .byte '0'|<(ROMPCT / 10 .MOD 10)
  .byte '.','0'|<(ROMPCT .MOD 10),"% full"
  .byte 0

title1_chr_pb53:
  .incbin "obj/nes/title1.chr.pb53"
  .incbin "obj/nes/title_cursor_sprites.chr.pb53"
title1_nam_pb53:
  .incbin "obj/nes/title1.nam.pb53"
titlepal=*-1
  mbyt "   101629 ff101229 ff103729 ff101722 ff101629"
title_pushstart_msg:
  .byte "Press Start Button",0
title_copr_msg:
  .byte $81," 2014 Damian Yerrick",0
title_options:
  .addr title_2pvs_msg, title_2pvslong_msg, title_2pcreative_msg
  .addr title_instructions_msg
NUM_TITLE_OPTIONS = (* - title_options) / 2
title_2pvs_msg:
  .byte "2 Players ", '0'|NUM_ROUNDS_TO_WIN, " Rounds",0
title_2pvslong_msg:
  .byte "2 Players 10 Rounds",0
title_2pcreative_msg:
  .byte "2 Players Endless",0
title_instructions_msg:
  .byte "Help",0
rtw_table:
  .byte NUM_ROUNDS_TO_WIN, 10, <-1
