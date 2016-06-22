; music.s
; part of sound engine for Concentration Room, Thwaite, Zap Ruder,
; and RHDE

;;; Copyright (C) 2009-2011 Damian Yerrick
;
;   This program is free software; you can redistribute it and/or
;   modify it under the terms of the GNU General Public License
;   as published by the Free Software Foundation; either version 3
;   of the License, or (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to 
;     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;     Boston, MA  02111-1307, USA.
;
;   Visit http://www.pineight.com/ for more information.

.importzp psg_sfx_state
.import soundBSS
.import start_sound
.export music_playing
.export init_music, stop_music, update_music, update_music_ch
.export music_play_note
.include "musicseq.h"

; these are used by callbacks
.export rowsPerBeat, rowBeatPart, tempoCounterHi, tempoCounterLo

.ifndef SOUND_NTSC_ONLY
SOUND_NTSC_ONLY = 0
.endif
.if (!SOUND_NTSC_ONLY)
.importzp tvSystem
.endif

.ifndef MUSIC_USE_ROW_CALLBACK
MUSIC_USE_ROW_CALLBACK = 0
.endif
.if MUSIC_USE_ROW_CALLBACK
.import music_row_callback, music_dalsegno_callback
.endif

; psg_sfx_state:
;       +0              +1              +2              +3
; $00 | Sq1 sound effect data ptr       Sq1 envelope data ptr
; $04-$0C repeat for Sq2, Tri, Noise
; $10 | Sq1 music pattern data ptr      Conductor data ptr
; $14 | Sq2 music pattern data ptr      Unused
; $18-$20 repeat for Tri, Noise, Attack

; soundBSS:
;       +0              +1              +2              +3
; $00-$0F Sound effect state for Sq1, Sq2, Tri, Noise
; $00 | Effect rate     Rate counter    Last period MSB Effect length
; $10-$1F Instrument envelope state for Sq1, Sq2, Tri, Noise
; $10 | Attack length   Attack pitch    Sustain vol     Note pitch
; $20-$2F Instrument arpeggio state
; $20 | Legato enable   Arpeggio phase  Arp interval 1  Arp interval 2
; $30-$43 Pattern reader state for Sq1, Sq2, Tri, Noise, Attack
; $20 | Note time left  Instrument ID   Pattern ID      Transpose amt
; $34 | Attack ch #     Unused          Rows per beat   Offset in beat
; $38-$3F Conductor state

noteAttackPos = psg_sfx_state + 2
musicPatternPos = psg_sfx_state + 16
conductorPos = psg_sfx_state + 30
attack_remainlen = soundBSS + 16
attackPitch = soundBSS + 17
noteEnvVol = soundBSS + 18
notePitch = soundBSS + 19
noteLegato = soundBSS + 32
arpPhase = soundBSS + 33
arpInterval1 = soundBSS + 34
arpInterval2 = soundBSS + 35
noteRowsLeft = soundBSS + 48
noteInstrument = soundBSS + 49
musicPattern = soundBSS + 50
patternTranspose = soundBSS + 51
attackChannel = soundBSS + 68
unused69 = soundBSS + 69
rowsPerBeat = soundBSS + 70
rowBeatPart = soundBSS + 71
tempoCounterLo = soundBSS + 72
tempoCounterHi = soundBSS + 73
music_tempoLo = soundBSS + 74
music_tempoHi = soundBSS + 75
conductorSegno = soundBSS + 76
conductorWaitRows = soundBSS + 78
music_playing = soundBSS + 79

FRAMES_PER_MINUTE_PAL = 3000
FRAMES_PER_MINUTE_NTSC = 3606

.segment "RODATA"

fpmLo:
  .byt <FRAMES_PER_MINUTE_NTSC, <FRAMES_PER_MINUTE_PAL
fpmHi:
  .byt >FRAMES_PER_MINUTE_NTSC, >FRAMES_PER_MINUTE_PAL
invfpm:
  .byt 8192*48/FRAMES_PER_MINUTE_NTSC, 8192*48/FRAMES_PER_MINUTE_PAL

silentPattern:
  .byt 26*8+7, 255
  
durations:
  .byt 1, 2, 3, 4, 6, 8, 12, 16
invdurations:
  .byt 48/1, 48/2, 48/3, 48/4, 48/6, 48/8, 48/12, 48/16

.segment "CODE"
.proc init_music
  asl a
  tax
  lda songTable,x
  sta conductorPos
  sta conductorSegno
  lda songTable+1,x
  sta conductorPos+1
  sta conductorSegno+1

  ldx #16
  bne channelLoopSkipHWOnly
  channelLoop:
    lda #0
    sta noteEnvVol,x
    channelLoopSkipHWOnly:
    lda #<silentPattern
    sta musicPatternPos,x
    lda #>silentPattern
    sta musicPatternPos+1,x
    sta musicPattern,x  ; bit 7 set: no pattern playing
    lda #0
    sta arpInterval1,x
    sta noteLegato,x
    sta arpPhase,x
    sta arpInterval2,x
    sta patternTranspose,x
    sta noteInstrument,x
    sta noteRowsLeft,x
    sta attack_remainlen,x
    dex
    dex
    dex
    dex
    bpl channelLoop
  ; A is still 0
  sta conductorWaitRows
  sta attackChannel
  lda #4
  sta rowsPerBeat
  lda #$FF
  sta rowBeatPart
  sta tempoCounterLo
  sta tempoCounterHi
  lda #<300
  sta music_tempoLo
  lda #>300
  sta music_tempoHi
.endproc
.proc resume_music
  lda #1
  sta music_playing
  rts
.endproc

.proc stop_music
  lda #0
  sta music_playing
  rts
.endproc

.proc update_music
  lda music_playing
  beq music_not_playing
  lda music_tempoLo
  clc
  adc tempoCounterLo
  sta tempoCounterLo
  lda music_tempoHi
  adc tempoCounterHi
  sta tempoCounterHi
  bcs new_tick
music_not_playing:
  rts
new_tick:

.if ::SOUND_NTSC_ONLY
  ldy #0
.else
  ldy tvSystem
  beq is_ntsc_1
  ldy #1
is_ntsc_1:
.endif

  ; Subtract tempo
  lda tempoCounterLo
  sbc fpmLo,y
  sta tempoCounterLo
  lda tempoCounterHi
  sbc fpmHi,y
  sta tempoCounterHi
  
  ; Update row
  ldy rowBeatPart
  iny
  cpy rowsPerBeat
  bcc :+
  ldy #0
:
  sty rowBeatPart
  

.if ::MUSIC_USE_ROW_CALLBACK
  jsr music_row_callback
.endif

  lda conductorWaitRows
  beq doConductor
  dec conductorWaitRows
  jmp skipConductor

doConductor:

  ldy #0
  lda (conductorPos),y
  inc conductorPos
  bne :+
    inc conductorPos+1
  :
  sta 0
  cmp #CON_SETTEMPO
  bcc @notTempoChange
  cmp #CON_SETBEAT
  bcc @isTempoChange
    and #%00000111
    tay
    lda durations,y
    sta rowsPerBeat
    ldy #0
    sty rowBeatPart
    jmp doConductor
  @isTempoChange:
    and #%00000111
    sta music_tempoHi
  
    lda (conductorPos),y
    inc conductorPos
    bne :+
      inc conductorPos+1
    :
    sta music_tempoLo
    jmp doConductor
  @notTempoChange:
  cmp #CON_WAITROWS
  bcc conductorPlayPattern
  beq conductorDoWaitRows
  
  cmp #CON_ATTACK_SQ1
  bcc @notAttackSet
    and #%00000011
    asl a
    asl a
    sta attackChannel
    jmp doConductor
  @notAttackSet:

  cmp #CON_FINE
  bne @notFine
    lda #0
    sta music_playing
    sta music_tempoHi
    sta music_tempoLo
.if ::MUSIC_USE_ROW_CALLBACK
    clc
    jmp music_dalsegno_callback
.else
    rts
.endif
  @notFine:

  cmp #CON_SEGNO
  bne @notSegno
    lda conductorPos
    sta conductorSegno
    lda conductorPos+1
    sta conductorSegno+1
    jmp doConductor
  @notSegno:

  cmp #CON_DALSEGNO
  bne @notDalSegno
    lda conductorSegno
    sta conductorPos
    lda conductorSegno+1
    sta conductorPos+1
.if ::MUSIC_USE_ROW_CALLBACK
    sec
    jsr music_dalsegno_callback
.endif
    jmp doConductor
  @notDalSegno:
  
  jmp skipConductor

conductorPlayPattern:
  and #$07
  asl a
  asl a
  tax
  lda #0
  sta noteLegato,x  ; start all patterns with legato off
  sta noteRowsLeft,x
  lda (conductorPos),y
  sta musicPattern,x
  iny
  lda (conductorPos),y
  sta patternTranspose,x
  iny
  lda (conductorPos),y
  sta noteInstrument,x
  tya
  sec
  adc conductorPos
  sta conductorPos
  bcc :+
    inc conductorPos+1
  :
  jsr startPattern
  jmp doConductor

  ; this should be last so it can fall into skipConductor
conductorDoWaitRows:
  lda (conductorPos),y
  inc conductorPos
  bne :+
    inc conductorPos+1
  :
  sta conductorWaitRows

skipConductor:

  ldx #12
  channelLoop:
    jsr processTrackPattern
    dex
    dex
    dex
    dex
    bpl channelLoop
  ldx #16
  ; fall through
  
processTrackPattern:
  lda noteRowsLeft,x
  beq anotherPatternByte
skipNote:
  dec noteRowsLeft,x
  rts

anotherPatternByte:
  lda (musicPatternPos,x)
  cmp #255
  bne notStartPatternOver
    jsr startPattern
    lda (musicPatternPos,x)
  notStartPatternOver:
    
  inc musicPatternPos,x
  bne patternNotNewPage
    inc musicPatternPos+1,x
  patternNotNewPage:

  cmp #INSTRUMENT
  bcc notExtraCommand
  beq isInstrument
    cmp #ARPEGGIO
    bne notArpeggio
    lda (musicPatternPos,x)
    lsr a
    lsr a
    lsr a
    lsr a
    sta arpInterval1,x
    lda (musicPatternPos,x)
    and #$0F
    sta arpInterval2,x
    jmp nextPatternByte
  notArpeggio:
    cmp #LEGATO_ON+1
    bcs notLegato
    cpx #16
    bcs anotherPatternByte
      and #$01
      sta noteLegato,x
      bmi anotherPatternByte
  notLegato:
    cmp #TRANSPOSE
    bne anotherPatternByte
    clc
    lda patternTranspose,x
    adc (musicPatternPos,x)
    sta patternTranspose,x
    jmp nextPatternByte
  isInstrument:
    lda (musicPatternPos,x)
    sta noteInstrument,x
  nextPatternByte:
    inc musicPatternPos,x
    bne anotherPatternByte
    inc musicPatternPos+1,x
    jmp anotherPatternByte
  notExtraCommand:
  
  ; set the note's duration
  pha
  and #$07
  tay
  lda durations,y
  sta noteRowsLeft,x
  pla
  lsr a
  lsr a
  lsr a
  cmp #25
  bcc isTransposedNote
  beq notKeyOff
    lda #0
    sta attack_remainlen,x
    cpx #16
    bcs notKeyOff
    sta noteEnvVol,x
  notKeyOff:
  jmp skipNote

  isTransposedNote:
    cpx #12
    beq isDrumNote
    clc
    adc patternTranspose,x
    ldy noteInstrument,x
    jsr music_play_note
    jmp skipNote

isDrumNote:
  stx 5
  asl a
  pha
  tax
  lda drumSFX,x
  jsr start_sound
  pla
  tax
  lda drumSFX+1,x
  bmi noSecondDrum
  jsr start_sound
noSecondDrum:
  ldx 5
  jmp skipNote

startPattern:
  lda musicPattern,x
  asl a
  bcc @notSilentPattern
    lda #<silentPattern
    sta musicPatternPos,x
    lda #>silentPattern
    sta musicPatternPos+1,x
    rts
  @notSilentPattern:
  tay
  lda musicPatternTable,y
  sta musicPatternPos,x
  lda musicPatternTable+1,y
  sta musicPatternPos+1,x
  rts
.endproc

;;
; Plays note A on channel X (0, 4, 8, 12) with instrument Y.
; Trashes 0-1
.proc music_play_note
notenum = 0
instrument_id = 1

  sta notenum
  sty instrument_id
  tya
  asl a
  asl a
  adc instrument_id
  tay
  
  ; at this point:
  ; x = channel #
  ; y = offset in instrument table
  cpx #16
  bcs skipSustainPart
    lda notenum
    sta notePitch,x
    lda noteLegato,x
    bne skipAttackPart
    lda #0
    sta arpPhase,x
    lda instrument_id
    sta noteInstrument,x
    lda instrumentTable,y
    asl a
    asl a
    asl a
    asl a
    ora #$0C
    sta noteEnvVol,x
  skipSustainPart:

  lda instrumentTable+4,y
  beq skipAttackPart
    txa
    pha
    cpx #16
    bcc notAttackChannel
      ldx attackChannel
      lda #$80  ; Disable arpeggio until sustain
      sta arpPhase,x
    notAttackChannel:
    lda notenum
    sta attackPitch,x
    lda instrumentTable+4,y
    sta noteAttackPos+1,x
    lda instrumentTable+3,y
    sta noteAttackPos,x
    lda instrumentTable+2,y
    and #$7F
    sta attack_remainlen,x
    pla
    tax
  skipAttackPart:
  rts
.endproc

.proc update_music_ch
xsave = 0
ysave = 1
out_volume = 2
out_pitch = 3

  lda music_playing
  beq silenced
  lda attack_remainlen,x
  beq noAttack
  dec attack_remainlen,x
  lda (noteAttackPos,x)
  inc noteAttackPos,x
  bne :+
  inc noteAttackPos+1,x
:
  sta out_volume
  lda (noteAttackPos,x)
  inc noteAttackPos,x
  bne :+
  inc noteAttackPos+1,x
:
  clc
  adc attackPitch,x
  ldy arpPhase,x
  bmi storePitchNoArpeggio
storePitchWithArpeggio:
  sta out_pitch
  stx xsave
  lda #$7F
  and arpPhase,x
  tay
  beq bumpArpPhase

  ; So we're in a nonzero phase.  Load the interval.
  clc
  adc xsave
  tax
  lda arpInterval1-1,x

  ; If phase 2's interval is 0, cycle through two phases (1, 2)
  ; instead of three (0, 1, 2).
  bne bumpArpPhase
  cpy #2
  bcc bumpArpPhase
  ldy #0
bumpArpPhase:
  iny
  cpy #3
  bcc noArpRestart
  ldy #0
noArpRestart:

  ; At this point, A is the arpeggio interval and Y is the next phase
  clc
  adc out_pitch
  sta out_pitch
  ldx xsave
  tya
  sta arpPhase,x
  rts

storePitchNoArpeggio:
  sta out_pitch
  rts

noAttack:
  lda noteEnvVol,x
  lsr a
  lsr a
  lsr a
  lsr a
  bne notSilenced
silenced:
  lda #0
  sta out_volume
  rts
notSilenced:
  sta out_volume
  lda noteInstrument,x
  asl a
  asl a
  adc noteInstrument,x
  tay  
  lda out_volume
  eor instrumentTable,y
  and #$0F
  eor instrumentTable,y
  sta out_volume
  lda noteEnvVol,x
  sec
  sbc instrumentTable+1,y
  bcc silenced
  sta noteEnvVol,x
  lda notePitch,x
  sty ysave
  jsr storePitchWithArpeggio
  ldy ysave

  ; bit 7 of attribute 2: cut note when half a row remains
  lda instrumentTable+2,y
  bpl notCutNote
  lda noteRowsLeft,x
  bne notCutNote

  clc
  lda tempoCounterLo
  adc #<(FRAMES_PER_MINUTE_NTSC/2)
  lda tempoCounterHi
  adc #>(FRAMES_PER_MINUTE_NTSC/2)
  bcc notCutNote
  
  ; if the next byte in the pattern is a tie or a legato enable
  lda (musicPatternPos,x)
  cmp #LEGATO_ON
  beq notCutNote
  cmp #LEGATO_OFF
  beq yesCutNote
  and #$F8
  cmp #N_TIE
  beq notCutNote
  lda noteLegato,x
  bne notCutNote
yesCutNote:
  lda #0
  sta noteEnvVol,x

notCutNote:
  rts

.endproc

