;
; Rampart style game for NES
; Copyright 2014-2018 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

; Sometime between ca65 2.13.2 and 2.14.0, the directory search
; rules for .include changed from the current working directory to
; the directory containing the file that was included, possibly
; to more closely match those of common C compilers.  So if you
; get "No such file or directory" here, you need newer ca65,
; or you need to add src to the include path in the makefile.
; The change appears not to have affected .incbin though.
.include "nes.inc"
.include "global.inc"
.include "mbyt.inc"

.export psg_sfx_state
.export INITIAL_CASH:absolute
OAM = $0200

; debug/release behavior switches
.if 0
  SHOW_RENT_RESULT = 0
  SHOW_RACESEL = 0
  INITIAL_CASH = 500
  SELECT_SPEEDUP = 1
  DRAW_DEBUGHEX = 1
  NUM_ROUNDS_TO_WIN = 2
.else
  SHOW_RENT_RESULT = 1
  SHOW_RACESEL = 1
  INITIAL_CASH = 0
  SELECT_SPEEDUP = 0
  DRAW_DEBUGHEX = 0
  NUM_ROUNDS_TO_WIN = 5
.endif

.segment "ZEROPAGE"
nmis:          .res 1
tvSystem:      .res 1
time_subtenths:.res 1
time_tenths:   .res 1
time_seconds:  .res 1
phase_seconds: .res 1
rounds_so_far: .res 1
oam_used:      .res 1  ; starts at 0
cur_turn:      .res 1
cur_keys:      .res 2
new_keys:      .res 2
das_keys:      .res 2
das_timer:     .res 2
cursor_x:      .res 2
cursor_y:      .res 2
cash_lo:       .res 2
cash_hi:       .res 2
player_race:   .res 2
hra_wins:      .res 2
debughex:      .res 2
psg_sfx_state: .res 36

.segment "INESHDR"
  .byt "NES",$1A  ; magic signature
  .byt 2          ; PRG ROM size in 16384 byte units
  .byt 0          ; CHR ROM size in 8192 byte units
  .byt $01        ; mirroring type and mapper number lower nibble
  .byt $00        ; mapper number upper nibble

.segment "VECTORS"
.addr nmi, reset, irq

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.
.proc nmi
  inc nmis
  ; falls through
.endproc
.proc irq
  rti
.endproc

; 
.proc reset
  ; The very first thing to do when powering on is to put all sources
  ; of interrupts into a known state.
  sei             ; Disable interrupts
  ldx #$00
  stx PPUCTRL     ; Disable NMI and set VRAM increment to 32
  stx PPUMASK     ; Disable rendering
  stx $4010       ; Disable DMC IRQ
  dex             ; Subtracting 1 from $00 gives $FF, which is a
  txs             ; quick way to set the stack pointer to $01FF
  bit PPUSTATUS   ; Acknowledge stray vblank NMI across reset
  bit SNDCHN      ; Acknowledge DMC IRQ
  lda #$40
  sta P2          ; Disable APU Frame IRQ
  lda #$0F
  sta SNDCHN      ; Disable DMC playback, initialize other channels

vwait1:
  bit PPUSTATUS   ; It takes one full frame for the PPU to become
  bpl vwait1      ; stable.  Wait for the first frame's vblank.

  ; Now put the system into a known state.

  ; Turn off decimal mode for compatibility with some famiclones
  cld
  
  ; Clear the zero page
  ldx #0
  txa
clear_zp:
  sta $00,x
  inx
  bne clear_zp
  lda #$C0
  sta debughex+0
  lda #$DE
  sta debughex+1
  lda #1
  sta player_race+0
  asl a
  sta player_race+1
  
  ; Other things to do here (not shown):
  ; Set up PRG RAM
  ; Copy initial high scores, bankswitching trampolines, etc. to RAM
  ; Set up initial CHR banks
  jsr init_sound

vwait2:
  bit PPUSTATUS  ; After the second vblank, we know the PPU has
  bpl vwait2     ; fully stabilized.

  ; From here on out, vsync with NMI
  lda #VBLANK_NMI
  sta PPUCTRL
  jsr getTVSystem
  sta tvSystem
  ; fall through
.endproc
.proc from_the_title
  jsr title_screen
  .if ::SHOW_RACESEL
    jsr racesel
  .endif
  jsr load_main_palette
  jsr load_main_chr
  jsr init_playfield

  ; Make the road using bits 0-3 of elapsed frames as a random seed
  lda nmis  ; Bits 0-3
  and #$03
  jsr make_road
  jsr place_initial_houses
  jsr lru4_init

  ldx #$00
  stx hra_wins+0
  stx hra_wins+1
  stx enclose_state
  stx enclose_turn
  stx rounds_so_far
  stx copy_region_left
  stx enclose_play_sfx
  jsr ppu_clear_oam

  ; Clear both nametables.  Primary is $20; secondary is $2C
  ; whose vast majority is offscreen.
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #TILE_BLACK
  ldy #$00
  sty PPUMASK
  ldx #$2C
  jsr ppu_clear_nt
  ldx #$20
  jsr ppu_clear_nt

  ; Draw the initial background
  jsr make_road_border
  lda #30
  sta copy_region_width

next_round:

  ; Remove tips of walls
  lda #DIRTY_RM_ENDS
  sta side_dirty+0
  sta side_dirty+1
  jsr encloop

  ; Connect the field and prepare to make doors
  jsr connect_field_start
:
  jsr connect_field_continue
  bcc :-
  lda rounds_so_far
  beq skip_doors_phase

  ; Pick up furniture on wrong side of walls, based on walldata
  ; from flood fill
  ldx #1
pickupplayerloop:
  stx cur_turn
  jsr pickup_outdoor_furni
:
  jsr bgup_vsync
  jsr pickup_outdoor_furni_continue
  bcc :-
  ldx cur_turn
  dex
  bpl pickupplayerloop

  lda #DIRTY_DOORLESS
  sta side_dirty+0
  sta side_dirty+1
  jsr encloop

  ldx #0
  stx copy_region_left
  lda #30
  sta copy_region_width
  jsr delay_500

  ldy #<alert_doors
  lda #>alert_doors
  jsr prepare_alertbox
  ldy #30
  jsr delay_y_tenths
  jsr remove_alertbox
  jsr doors_phase
skip_doors_phase:
  jsr delay_500
  ldx #1
getrentloop:
  stx enclose_turn
  jsr count_rent_tiles
  ldx enclose_turn

  ; Award 2 cio per 8x8 tile
  lda 0
  sta cur_piece_lo,x  ; cur_piece: "Area" in alert.s
  asl a
  sta unitzp24,x      ; unitzp24: "Rent"
  lda 1
  sta cur_piece_hi,x
  rol a
  sta unitzp24+2,x
  
  ; And add this to the player's total
  lda unitzp24,x
  adc cash_lo,x
  sta cash_lo,x
  lda unitzp24+2,x
  adc cash_hi,x
  sta cash_hi,x
  dex
  bpl getrentloop

.if ::SHOW_RENT_RESULT
  ldy #<alert_rent_result
  lda #>alert_rent_result
  jsr prepare_alertbox

  ldy #20
  jsr delay_y_tenths
  jsr remove_alertbox
  jsr delay_500

  ; Now get ready to furnish
  ldy #<alert_furnish
  lda #>alert_furnish
  jsr prepare_alertbox
  ldy #20
  jsr delay_y_tenths
  jsr remove_alertbox
.endif

  jsr furnish_phase
  jsr delay_500
  ldx #1
hra_loop:
  stx enclose_turn
  jsr rate_furni
  ldx enclose_turn
hrascorelo = $0E
hrascorehi = $0F
  lda hrascorelo
  sta unitzp24+0,x
  lda hrascorehi
  sta unitzp24+2,x
  dex
  bpl hra_loop

  lda #COLOR_GOODGRASS
  sta player_grass_color+0
  sta player_grass_color+1

  ; Add a win to the player with higher HRA score
  lda unitzp24+3
  cmp unitzp24+2
  bne :+
  lda unitzp24+1
  cmp unitzp24+0
  beq drawn_round
:
  lda #0
  rol a
  tax
  inc hra_wins,x
  eor #$01
  tax
  lda #COLOR_BADGRASS
  sta player_grass_color,x
drawn_round:

  ldy #<alert_furnish_result
  lda #>alert_furnish_result
  jsr prepare_alertbox
  ldy #40
  jsr delay_y_tenths
  jsr remove_alertbox
  jsr delay_500

  lda rounds_to_win
  bmi not_won_yet
  lda hra_wins+0
  cmp rounds_to_win
  bcs won_yet
  lda hra_wins+1
  cmp rounds_to_win
  bcc not_won_yet
won_yet:
  jmp game_won
not_won_yet:

  ; Prepare for battle
  ldy #<alert_battle
  lda #>alert_battle
  jsr prepare_alertbox
  ldy #20
  jsr delay_y_tenths
  jsr remove_alertbox
  ; The battle music here is "ironic" because a gang fight breaking
  ; out is sort of unexpected in this situation.
  lda #MUSIC_IRONIC_BATTLE
  jsr init_music
  jsr battle_phase
  jsr stop_music

  ; Now disconnect and get ready to build
  jsr disconnect_field_start
:
  jsr disconnect_field_continue
  bcc :-
  ldx #0
  stx copy_region_left
  lda #30
  sta copy_region_width
  lda #DIRTY_ENCLOSE
  sta side_dirty+0
  sta side_dirty+1
  jsr delay_500
  ldy #<alert_build
  lda #>alert_build
  jsr prepare_alertbox
  ldy #20
  jsr delay_y_tenths
  lda #MUSIC_BUILD
  jsr init_music
  ldy #20
  jsr delay_y_tenths
  jsr remove_alertbox

  jsr build_phase
  inc rounds_so_far
  jmp next_round
.endproc

.proc game_won
  jmp from_the_title
.endproc

.proc encloop
  jsr enclose_run_step
  lda nmis
:
  cmp nmis
  beq :-
  jsr update_sound
  lda side_dirty+0
  ora side_dirty+1
  and #(DIRTY_DOORLESS|DIRTY_RM_ENDS|DIRTY_ENCLOSE)
  ora enclose_state
  bne encloop
  rts
.endproc

.proc delay_500
  ldy #5
.endproc
.proc delay_y_tenths
  sty phase_seconds
loop:
  jsr update_game_time
  jsr enclose_run_step
  lda time_subtenths
  bne :+
  dec phase_seconds
:
  jsr build_choose_redraw
  jsr bgup_vsync
  jsr update_sound
  lda phase_seconds
  bne loop
  jmp read_pads
.endproc

.proc update_game_time
  .if ::SELECT_SPEEDUP
    lda cur_keys+0
    and #KEY_SELECT
    bne new_tenth
  .endif

  inc time_subtenths
  lda time_subtenths
  cmp #5
  beq new_tenth_if_pal
  bcs new_tenth
  rts
new_tenth_if_pal:
  lda tvSystem
  beq no_new_tenth
new_tenth:
  lda #0
  sta time_subtenths
  inc time_tenths
  lda time_tenths
  cmp #10
  bcc no_new_tenth
  lda #0
  sta time_tenths
  inc time_seconds
no_new_tenth:
  rts
.endproc

;;
; Updates game time and decrements seconds left in phase.
; If phase_seconds = 0, phase_seconds is left unchanged.
; @return C is clear only on first frame of seconds 5, 4, 3, 2, 1
.proc countdown_logic
  lda new_keys
  ora new_keys+1
  and #KEY_START
  beq not_pause
  ldx #0
  ldy #0
pauseoamloop:
  lda #112
  sta OAM+0,x
  lda pausedtiles,y
  sta OAM+1,x
  lda #0
  sta OAM+2,x
  txa
  asl a
  adc #(256-56)/2
  sta OAM+3,x
  inx
  inx
  inx
  inx
  iny
  cpy #7
  bcc pauseoamloop
  jsr ppu_clear_oam
stillpaused:
  lda nmis
:
  cmp nmis
  beq :-
  dec nmis
  lda #>OAM
  sta OAM_DMA
  lda #OBJ_ON
  sta PPUMASK
  jsr read_pads
  jsr update_sound
  lda new_keys
  ora new_keys+1
  and #KEY_START
  beq stillpaused

not_pause:
  ; Timer logic
  jsr update_game_time
  sec
  lda time_subtenths
  ora time_tenths
  bne not_54321
  lda phase_seconds
  beq not_54321
  dec phase_seconds
  beq not_54321
  lda phase_seconds
  cmp #6
not_54321:
  rts
.pushseg
.segment "RODATA"
pausedtiles: mbyt "30310A32330E0D" ; || P A U S E D
.popseg
.endproc


.proc draw_debughex
.if ::DRAW_DEBUGHEX
  ldy oam_used
  ldx #112
  lda debughex+0
  jsr do2dig
  lda debughex+1
  jsr do2dig
  sty oam_used
  rts
do2dig:
  pha
  lsr a
  lsr a
  lsr a
  lsr a
  jsr do1dig
  pla
  and #$0F
do1dig:
  sta OAM+1,y
  lda #FIELDTOP_Y-1+8*FIELD_HT
  sta OAM+0,y
  lda #0
  sta OAM+2,y
  clc
  txa
  sta OAM+3,y
  adc #8
  tax
  tya
  adc #4
  tay
.endif
  rts
.endproc

.proc load_main_chr
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  sta PPUADDR
  sta PPUADDR
  lda #>bggfx_pb53
  ldy #<bggfx_pb53
  ldx #128*BGGFX_HT/256
  jsr load_blk_chr
  lda #$10
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #>spritegfx_pb53
  ldy #<spritegfx_pb53
  ldx #128*SPRITEGFX_HT/256
  jsr load_blk_chr
  lda #$15
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #>furni_sprites_pb53
  ldy #<furni_sprites_pb53
  ldx #128*24/256
  jsr load_blk_chr

  ; sprites A0-CF: player 1 tiles
  ; sprites D0-FF: player 2 tiles
  lda #$1A
  sta PPUADDR
  lda #$00
  sta PPUADDR
  ldx player_race+0
  jsr do1race
  ldx player_race+1
do1race:
  lda race_sprites_hi,x
  ldy race_sprites_lo,x
  ldx #128*24/512
.endproc
;;
; Decompresses Donut data to the current position in CHR RAM.
; @param A high byte of Donut address
; @param Y low byte of Donut address
; @param X size of decompressed data in units of 64 bytes,
; 4 tiles, or 256 pixels
.proc load_blk_chr
block_count = $04  ; matches variable donut_block_count
  sta ciSrc+1
  sty ciSrc
  stx block_count
  eightmoretiles:
    jsr donut_decompress_block
    ldx #0
    copyloop:
      lda PB53_outbuf,x
      sta PPUDATA
      inx
      cpx #64
    bcc copyloop
    lda block_count
  bne eightmoretiles
rts
.endproc

.proc load_main_palette
  ; seek to the start of palette memory ($3F00-$3F1F)
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$3F
  ldx #$00
  stx PPUMASK
  sta PPUADDR
  stx PPUADDR
copypalloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #32
  bcc copypalloop
  rts
.endproc

.segment "RODATA"
BLACK  = $FF
WHITE  = $20
PALE   = $38
TEAM1C = $16
TEAM2C = $12
GRASSC = COLOR_GOODGRASS
ROADC  = $00

initial_palette:
  .byte BLACK,PALE,GRASSC,TEAM1C
  .byte BLACK,PALE,GRASSC,TEAM2C
  .byte BLACK,PALE,GRASSC,ROADC
  .byte BLACK,ROADC,ROADC,ROADC
  .byte BLACK,BLACK,TEAM1C,WHITE
  .byte BLACK,BLACK,TEAM2C,WHITE
  .byte BLACK,ROADC,ROADC,ROADC
  .byte BLACK,BLACK,BLACK,PALE  ; for corners

BGGFX_HT = 64
SPRITEGFX_HT = 32

bggfx_pb53:
  .incbin "obj/nes/bggfx.chr.donut"
spritegfx_pb53:
  .incbin "obj/nes/spritegfx.chr.donut"
furni_sprites_pb53:
  .incbin "obj/nes/furni_sprites.chr.donut"
nander_sprites_pb53:
  .incbin "obj/nes/nander_sprites.chr.donut"
poli_sprites_pb53:
  .incbin "obj/nes/poli_sprites.chr.donut"
volci_sprites_pb53:
  .incbin "obj/nes/volci_sprites.chr.donut"
race_sprites_lo:
  .lobytes nander_sprites_pb53, poli_sprites_pb53, volci_sprites_pb53
race_sprites_hi:
  .hibytes nander_sprites_pb53, poli_sprites_pb53, volci_sprites_pb53

