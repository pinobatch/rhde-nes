; Constant and macro dDefinitions for Pently 4
; Copyright 2010-2014 Damian Yerrick
;
; License: zlib

.global pently_drums, pently_sfx_table
.global pently_patterns, pently_instruments, pently_songs
.globalzp NUM_SONGS

; Sound effect/drum definitions
.macro sfxdef baseaddr, length, period, channel
  .addr baseaddr
  .byt (channel<<2)|((period - 1)<<4)
  .byt length
.endmacro

; Pattern commands
N_C  =  0*8
N_CS =  1*8
N_D  =  2*8
N_DS =  3*8
N_E  =  4*8
N_F  =  5*8
N_FS =  6*8
N_G  =  7*8
N_GS =  8*8
N_A  =  9*8
N_AS = 10*8
N_B  = 11*8
N_DB = N_CS
N_EB = N_DS
N_GB = N_FS
N_AB = N_GS
N_BB = N_AS
N_CH  = N_C  + 12*8
N_CSH = N_CS + 12*8
N_DBH = N_DB + 12*8
N_DH  = N_D  + 12*8
N_DSH = N_DS + 12*8
N_EBH = N_EB + 12*8
N_EH  = N_E  + 12*8
N_FH  = N_F  + 12*8
N_FSH = N_FS + 12*8
N_GBH = N_GB + 12*8
N_GH  = N_G  + 12*8
N_GSH = N_GS + 12*8
N_ABH = N_AB + 12*8
N_AH  = N_A  + 12*8
N_ASH = N_AS + 12*8
N_BBH = N_BB + 12*8
N_BH  = N_B  + 12*8
N_CHH = N_CH + 12*8
N_TIE = 25*8
REST  = 26*8
INSTRUMENT = $D8
ARPEGGIO = $D9
LEGATO_OFF = $DA
LEGATO_ON = $DB
TRANSPOSE = $DC
PATEND = $FF

; The default duration is one row (a sixteenth note in the tracker).
; OR the pitch with one of these constants.
D_8  = 1
D_D8 = 2
D_4  = 3
D_D4 = 4
D_2  = 5
D_D2 = 6
D_1  = 7

CON_PLAYPAT = $00   ; next: pattern, transpose (if not drums), instrument (if not drums)
CON_WAITROWS = $20  ; next: number of rows to wait minus 1
CON_FINE = $21      ; stop music now
CON_SEGNO = $22     ; set loop point
CON_DALSEGNO = $23  ; jump to loop point. if no point was set, jump to start of song.
CON_ATTACK_SQ1 = $24
CON_ATTACK_SQ2 = $25
CON_ATTACK_TRI = $26
CON_SETTEMPO = $30  ; low bits: bits 10-8 of tempo in rows/min; next: bits 7-0 of tempo
CON_SETBEAT = $38  ; low bits: duration type (D_something) corresponding to one beat

; Conductor macros
.macro playPatSq1 patid, transpose, instrument
  .byt CON_PLAYPAT|0, patid, transpose, instrument
.endmacro
.macro playPatSq2 patid, transpose, instrument
  .byt CON_PLAYPAT|1, patid, transpose, instrument
.endmacro
.macro playPatTri patid, transpose, instrument
  .byt CON_PLAYPAT|2, patid, transpose, instrument
.endmacro
.macro playPatNoise patid, transpose, instrument
  .byt CON_PLAYPAT|3, patid, transpose, instrument
.endmacro
.macro playPatAttack patid, transpose, instrument
  .byt CON_PLAYPAT|4, patid, transpose, instrument
.endmacro
.macro stopPatSq1
  .byt CON_PLAYPAT|0, 255, 0, 0
.endmacro
.macro stopPatSq2
  .byt CON_PLAYPAT|1, 255, 0, 0
.endmacro
.macro stopPatTri
  .byt CON_PLAYPAT|2, 255, 0, 0
.endmacro
.macro stopPatNoise
  .byt CON_PLAYPAT|3, 255, 0, 0
.endmacro
.macro stopPatAttack
  .byt CON_PLAYPAT|4, 255, 0, 0
.endmacro
.macro waitRows n
  .byt CON_WAITROWS, n-1
.endmacro
.macro fine
  .byt CON_FINE
.endmacro
.macro segno
  .byt CON_SEGNO
.endmacro
.macro dalSegno
  .byt CON_DALSEGNO
.endmacro
.macro setTempo rowsPerMin
  .byt CON_SETTEMPO|>rowsPerMin, <rowsPerMin
.endmacro
.macro setBeatDuration durCode
  .byt CON_SETBEAT|durCode
.endmacro
.macro attackOnSq1
  .byt CON_ATTACK_SQ1
.endmacro
.macro attackOnSq2
  .byt CON_ATTACK_SQ2
.endmacro
.macro attackOnTri
  .byt CON_ATTACK_TRI
.endmacro

