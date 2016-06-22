;
; Music sequence data for RHDE
; Copyright 2009-2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
; Translation: Go ahead and make your ReMixes, but credit me.

.include "musicseq.h"
.segment "RODATA"

psg_sound_table:
  sfxdef turn_snd,     6, 1, 0
  sfxdef shift_snd,    2, 1, 0
  sfxdef land_snd,     8, 1, 0
  sfxdef line_snd,    15, 1, 0
  sfxdef climb_snd,   12, 1, 0
  sfxdef hold_piece_snd, 5, 2, 3
  sfxdef shoot_snd,    8, 5, 3
  sfxdef start1_snd,  16, 1, 0

  sfxdef explode_snd,  8, 4, 3
  sfxdef scanbeep_snd, 2, 6, 0
  
  ; discovered on 2014-05-09 that there's a bug in the sfx engine
  ; that stops a slowed sound effect after the first frame of its
  ; last step
  
  sfxdef kick_snd,     3, 1, 3
  sfxdef kick2_snd,    4, 1, 2
  sfxdef snare_snd,    7, 1, 3
  sfxdef snare2_snd,   2, 1, 2
  sfxdef hihat_snd,    2, 1, 3
  sfxdef openhat_snd, 13, 1, 3
  sfxdef snare_openhat_snd, 13, 1, 3

; alternating duty/volume and pitch bytes

turn_snd:
  .dbyt $4F24,$4424,$4F29,$4429,$4F2E,$442E
shift_snd:
  .dbyt $4F30,$4430
scanbeep_snd:
  .dbyt $8C2A,$832A
land_snd:
  .dbyt $8F13,$8D0F,$8B0C,$8909,$8707,$8505,$8303,$8102
line_snd:
  .dbyt $4F27,$4E2A,$4D2C
  .dbyt $4C27,$4B29,$4A2C
  .dbyt $8927,$882A,$872C
  .dbyt $8627,$8529,$842C
  .dbyt $8327,$822A,$812C
climb_snd:
  .byt $47,26,$49,29,$4B,31,$4D,32,$4F,33,$4F,33,$4F,34
  .byt $4F,34,$4B,34,$4B,34,$49,33,$47,31
hold_piece_snd:
  .dbyt $070B,$0A0A,$0A0B,$080C,$060E
shoot_snd:
  .dbyt $0A03,$0904,$0804,$0704,$0605,$0406,$0306,$0206
explode_snd:
  .dbyt $0F0D,$0D0D,$0B0D,$090E,$070E,$050E,$030e,$020e

start1_snd:
  .dbyt $8C32,$4932,$4632,$8432,$8232
  .dbyt $8C3E,$493E,$463E,$843E,$823E
  .dbyt $8C39,$4939,$4639,$8439,$8239,$8139

snare2_snd:
  .dbyt $8F26, $8F25
kick2_snd:
  .dbyt $8F1F, $8F1B, $8F18, $8215
hihat_snd:
  .dbyt $0603, $0483
snare_snd:
  .dbyt $0A05, $0884, $0604, $0484, $0304, $0204, $0104
kick_snd:
  .dbyt $0804, $080E, $040E
openhat_snd:
  .dbyt $0703, $0683, $0503, $0583, $0403, $0483
  .dbyt $0303, $0383, $0203, $0283, $0203, $0283, $0103
snare_openhat_snd:
  .dbyt $0A05, $0884, $0604, $0583, $0403, $0483
  .dbyt $0303, $0383, $0203, $0283, $0203, $0283, $0103

track2_attack:
  .dbyt $0700,$0B00,$0C00

; Each drum consists of one or two sound effects.
drumSFX:
  .byt 10, 11
  .byt 12, 13
  .byt 14, <-1
  .byt 15, <-1
  .byt 15, 10
  .byt 16, 13
KICK  = 0*8
SNARE = 1*8
CLHAT = 2*8
OHAT = 3*8
KICKOHAT = 4*8
SNAREOHAT = 5*8

instrumentTable:
  ; first byte: initial duty (0/4/8/c) and volume (1-F)
  ; second byte: volume decrease every 16 frames
  ; third byte:
  ; bit 7: cut note if half a row remains
  .byt $86, 0, $80  ; bass (with staccato)
  .addr 0
  .byt $48, 3, $80  ; substitute for ohit until arpeggio supported
  .addr 0

  ; 2-3: new melody
  .byt $09, 8, 0  ; new melody "ukulele"
  .addr 0
  .byt $47, 6, $00  ; bass assist
  .addr 0

  ; 4: game.ftm track2
  .byt $07, 3, $83  ; lmia part 1 string
  .addr track2_attack

songTable:
  .addr kalinka_conductor
  .addr track2_conductor

musicPatternTable:
kalinka_pats = (* - musicPatternTable) >> 1
  .addr kalinka_intro, kalinka_drumstart
  .addr kalinka_sq1, kalinka_bass, kalinka_drum
  .addr kalinka_drumfill, kalinka_B_sq1, kalinka_B_bass
  .addr kalinka_ending_sq1, kalinka_ending_sq2, kalinka_ending_tri
track2_pats = (* - musicPatternTable) >> 1
  .addr track2_sq2_A, track2_sq1_A, track2_sq1_Aend
  .addr track2_sq2_B, track2_sq1_B
  .addr track2_tri_A, track2_tri_B, track2_drums, track2_tri_Aend

;____________________________________________________________________
; Build theme: Kalinka

kalinka_conductor:
  setTempo 560
  playPatTri   kalinka_pats+0, 27, 0
  playPatSq2   kalinka_pats+0, 31, 0
  playPatSq1   kalinka_pats+0, 22, 0
  waitRows 28
  playPatNoise kalinka_pats+1,  0, 0
  waitRows 4
  playPatSq1   kalinka_pats+2, 15, 2
  playPatSq2   kalinka_pats+3,  3, 3
  playPatTri   kalinka_pats+3,  3, 0
  playPatNoise kalinka_pats+4,  0, 0
  waitRows 124
  stopPatSq1
  stopPatSq2
  stopPatTri
  playPatNoise kalinka_pats+5,  0, 0
  waitRows 4
  segno
  ; B part
  playPatSq1   kalinka_pats+6, 10, 0
  playPatSq2   kalinka_pats+7,  3, 3
  playPatTri   kalinka_pats+7,  3, 0
  playPatNoise kalinka_pats+4,  0, 0
  waitRows 64
  playPatSq1   kalinka_pats+8, 11, 0
  playPatSq2   kalinka_pats+9, 23, 0
  playPatTri   kalinka_pats+10,23, 0
  stopPatNoise
  waitRows 48 ;64
  fine

kalinka_intro:
  .byt N_EB|D_2, N_DB|D_2, N_C|D_D2, REST|D_4, PATEND
kalinka_drumstart:
  .byt SNAREOHAT|D_D4, PATEND

kalinka_sq1:
  .byt N_G|D_8, N_BB|D_8, N_G|D_8, N_BB|D_8
  .byt ARPEGGIO, $47, N_CHH|D_8, ARPEGGIO, $00
  .byt N_G|D_8, N_BB|D_8, N_G|D_8
  .byt N_E|D_8, N_E|D_8, N_G|D_8, N_BB|D_8, N_F|D_8, N_F|D_8, N_AB|D_8, N_CH|D_8
  .byt PATEND
  
kalinka_bass:
  .byt N_C|D_8, N_CH|D_8, N_C|D_8, N_BB|D_8, N_C|D_8, N_CH|D_8, N_C|D_8, N_CH|D_8
  .byt N_E|D_8, N_EH|D_8, N_E|D_8, N_EH|D_8, N_F|D_8, N_FH|D_8, N_AB|D_8, N_ABH|D_8
  .byt PATEND
kalinka_drum:
  .byt KICK|D_8, CLHAT|D_8, SNARE|D_8, CLHAT|D_8
  .byt KICK|D_8, CLHAT, CLHAT, SNARE|D_8, CLHAT|D_8
  .byt KICK|D_8, CLHAT|D_8, SNARE|D_8, CLHAT|D_8
  .byt KICK|D_8, CLHAT|D_8, SNARE, CLHAT, SNAREOHAT|D_8
  .byt PATEND
kalinka_drumfill:
  .byt SNAREOHAT, SNARE, SNARE, SNARE, PATEND

kalinka_B_sq1:
  .byt N_F|D_2, N_TIE|D_D4, N_EB|D_8, N_CH|D_2, N_DBH|D_2
  .byt PATEND
kalinka_B_bass:
  .byt N_F|D_8, N_FH|D_8, N_F|D_8, N_FH|D_8, N_F|D_8, N_FH|D_8, N_F|D_8, N_EBH|D_8
  .byt N_C|D_8, N_CH|D_8, N_C|D_8, N_CH|D_8, N_DB|D_8, N_DBH|D_8, N_DB|D_8, N_DBH|D_8
  .byt PATEND
kalinka_ending_sq1:
  .byt N_CH|D_1, N_DH|D_1, N_EH|D_1
  .byt PATEND
kalinka_ending_sq2:
  .byt N_E|D_1, N_A|D_2, N_B|D_2, N_CH|D_1
  .byt PATEND
kalinka_ending_tri:
  .byt N_F|D_1, N_G|D_1, N_C|D_1
  .byt PATEND

;____________________________________________________________________
; game.ftm #2
; Written by "Cinos" (Josh Vega)
; Sounds like the love child of Super Mario Bros. 2 and
; Sonic the Hedgehog 2's Casino Night Zone

track2_conductor:
  setTempo 468
  playPatNoise track2_pats+7, 15, 0
  playPatSq2 track2_pats+0, 15, 4
  playPatSq1 track2_pats+1, 15, 4
  playPatTri track2_pats+5, 15, 0
  waitRows 165
  playPatTri track2_pats+8, 15, 0
  waitRows 6
  playPatSq1 track2_pats+2, 15, 4
  waitRows 21
  playPatTri track2_pats+6, 15, 0
  playPatSq2 track2_pats+3, 27, 4
  playPatSq1 track2_pats+4, 27, 4
  waitRows 192
  dalSegno

track2_sq2_A:
  .byte N_EH|D_8, REST, N_GH|D_D8, N_TIE|D_8, N_CHH|D_D8, REST
  .byte N_EH|D_8, REST, N_G|D_8, REST, N_CH|D_8, REST, N_A|D_8, REST
  .byte N_B|D_8, REST, N_GSH|D_D8, N_TIE|D_8, N_DH|D_D8, REST
  .byte N_DH|D_D4, REST|D_D4
  .byte N_FH|D_8, N_DH, N_CH|D_8, N_FH|D_D8, REST, N_GSH|D_8, REST
  .byte N_CHH|D_8, REST, N_CHH|D_8, REST, N_GSH|D_8, REST, N_CHH|D_8, N_CHH|D_D8
  .byte REST, N_FH|D_8, REST, N_DH|D_8, REST, N_CH|D_8, N_B|D_D4
  .byte REST, N_B|D_D4
  .byte N_EH|D_8, REST, N_GH|D_D8, N_TIE|D_8, N_CHH|D_D8, REST
  .byte N_EH|D_8, REST, N_G|D_8, REST, N_CH|D_8, N_G, N_A|D_8, REST
  .byte N_B|D_8, REST, N_GSH|D_D8, N_TIE|D_8, N_DH|D_D8, REST
  .byte N_DH|D_D8, N_E|D_8, REST, N_GS|D_8, N_FS, N_E|D_8, REST
  .byte N_FH|D_8, N_DH, N_CH|D_8, N_FH|D_D8, REST, N_GSH|D_8, REST
  .byte N_CHH|D_8, REST, N_CHH|D_8, REST, N_GSH|D_8, REST, N_CHH|D_8, N_CHH|D_D8
  .byte REST, N_CHH|D_8, REST, N_GH|D_8, N_EH, N_FH|D_8, REST
  .byte N_EH|D_8, REST, N_GH|D_8, N_CHH, TRANSPOSE, 12, N_EH|D_8, TRANSPOSE, <-12, REST|D_4
  .byte PATEND

track2_sq1_A:
  .byte N_CH|D_8, REST, N_EH|D_D8, N_TIE|D_8, N_GH|D_D8, REST
  .byte N_CH|D_8, REST|D_D2
  .byte REST, N_DH|D_D8, N_TIE|D_8, N_EH|D_D8, REST
  .byte N_GS|D_D4, REST|D_D4
  .byte N_CH|D_8, N_B, N_A|D_8, N_CH|D_D8, REST, N_FH|D_8, REST
  .byte N_GSH|D_8, REST, N_GH|D_8, REST, N_FH|D_8, REST, N_GSH|D_8, N_GH|D_D8
  .byte REST, N_DH|D_8, REST, N_B|D_8, REST, N_FH|D_8, N_EH|D_D4
  .byte REST, N_DH|D_D4
  .byte PATEND

track2_sq1_Aend:
  .byte N_GH|D_8, REST, N_EH|D_8, N_CH, N_DH|D_8, REST
  .byte N_CH|D_8, REST, N_EH|D_8, N_GH, N_CHH|D_8, REST|D_4
  .byte PATEND
  
track2_tri_A:
  .byte N_CH|D_8, REST|D_D8, N_CH, N_G|D_8, REST|D_D8, N_G
  .byte N_CH|D_8, REST|D_D8, N_G, N_CH|D_8, REST, N_DH|D_8, REST
  .byte N_EH|D_8, REST|D_D8, N_EH, N_B|D_8, REST|D_D8, N_B
  .byte N_EH|D_8, REST|D_D8, N_B, N_EH|D_8, REST, N_GH|D_8, REST
  .byte N_FH|D_8, REST|D_D8, N_FH, N_CH|D_8, REST|D_D8, N_CH
  .byte N_FH|D_8, REST|D_D8, N_CH, N_FH|D_8, REST, N_GSH|D_8, REST
  .byte N_GH|D_8, REST|D_D8, N_GH, N_DH|D_8, REST|D_D8, N_DH
  .byte N_GH|D_8, N_GH, N_FH|D_8, REST, N_EH|D_8, REST, N_DH|D_8, REST
  .byte PATEND

track2_tri_Aend:
  .byte N_GH|D_8, REST
  .byte N_CH|D_8, REST|D_D8, N_CH, N_DH|D_8, REST|D_D8, N_GH
  .byte N_CH|D_8, REST, N_G|D_8, REST, N_CH|D_8, REST, N_DH|D_8, REST
  .byte PATEND

track2_sq2_B:
  .byte N_B|D_8, N_A, N_GS|D_8, N_B|D_4, N_DH|D_8, REST
  .byte N_GSH|D_8, REST, N_FH|D_8, REST, N_EH|D_8, REST, N_DH|D_8, N_EH|D_8
  .byte REST, N_CH|D_D8, REST, N_B|D_8, REST, N_DH|D_8, REST
  .byte N_EH|D_8, N_DH, N_CH|D_8, N_B, N_CH|D_8, N_B, N_A|D_8, N_G
  .byte N_FS|D_8, REST, N_A|D_8, REST, N_DH|D_8, REST, N_FSH|D_8, N_AH|D_8
  .byte REST, TRANSPOSE, +12, N_CH, N_DH|D_8, N_A, N_FS|D_8, N_G, N_A|D_8, N_FS, TRANSPOSE, <-12
  .byte N_GH|D_8, N_AH, N_BH|D_8, N_GH, N_FH|D_8, N_GH, N_AH|D_8, N_FH
  .byte N_EH|D_D4, N_DH|D_D4
  .byte PATEND

track2_sq1_B:
  .byte N_GS|D_8, N_FS, N_E|D_8, N_GS|D_4, N_B|D_8, REST
  .byte N_EH|D_8, REST, N_DH|D_8, REST, N_CH|D_8, REST, N_B|D_8, N_CH|D_8
  .byte REST, N_A|D_D8, REST, N_GS|D_8, REST, N_B|D_8, REST
  .byte N_CH|D_8, N_B, N_A|D_8, N_GS, N_A|D_8, N_G, N_E|D_8, N_C
  .byte N_D|D_8, REST, N_E|D_8, REST, N_FS|D_8, REST, N_A|D_8, N_DH|D_8
  .byte REST, N_EH, N_FSH|D_8, N_DH, N_CH|D_8, N_DH, N_EH|D_8, N_CH
  .byte N_B|D_8, N_CH, N_DH|D_8, N_B, N_A|D_8, N_B, N_CH|D_8, N_A
  .byte N_G|D_D4, REST|D_D4
  .byte PATEND

track2_tri_B:
  .byte N_EH|D_8, REST|D_D8, N_EH, N_B|D_8, REST|D_D8, N_B
  .byte N_EH|D_8, REST|D_D8, N_EH, N_B|D_8, REST, N_EH|D_8, REST
  .byte N_A|D_8, REST|D_D8, N_A, N_B|D_8, REST|D_D8, N_B
  .byte N_A|D_8, REST|D_D8, N_A, N_EH|D_8, REST, N_A|D_8, REST
  .byte N_DH|D_8, REST|D_D8, N_DH, N_A|D_8, REST|D_D8, N_A
  .byte N_DH|D_8, REST|D_D8, N_DH, N_A|D_8, REST, N_DH|D_8, REST
  .byte N_G|D_8, REST|D_D8, N_G, N_F|D_8, REST|D_D8, N_F
  .byte N_E|D_8, N_EH, N_CH|D_8, REST, N_DH|D_8, REST, N_G|D_8, REST
  .byte PATEND

track2_drums:
  .byte KICK|D_D8, CLHAT|D_8, CLHAT, KICK|D_D8, CLHAT|D_8, CLHAT
  .byte KICK|D_D8, CLHAT|D_8, CLHAT, KICK|D_D8, KICK|D_D8
  .byte PATEND
