;
; RHDE furnish phase: main loop and shop window
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "ram.h"

; Number of distinct one sided shapes
; (including F, R, T, V, W that aren't in Rampart)
NUM_PIECES = 18
; Number of shapes that appear twice in the initial set
REPEATED_PIECES = 4
; Number of shapes in the initial set
MIN_PIECES = 7
; When the player adds two to this many power pieces, another shape
; is added to r.  Decreasing this will make the big clunky pieces
; show up sooner.
DEALDEPTH_SHIFT = 1
.exportzp NUM_PIECES

; This is based on a routine by Greg Cook that implements
; a CRC-16 cycle in constant time, without tables.
; 39 bytes, 66 cycles, AXP clobbered, Y preserved.
; http://www.6502.org/source/integers/crc-more.html
; Setting seed to $FFFF and then taking
; CRC([$01 $02 $03 $04]) should evaluate to $89C3.

.segment "ZEROPAGE"
; Current value of CRC, not necessarily contiguous
CRCLO: .res 1
CRCHI: .res 1

.segment "BSS"
player_dealdepth: .res 2
lru_queue: .res 2 * (NUM_PIECES + REPEATED_PIECES)

.segment "CODE"

; If using CRC as a PRNG, use this entry point
.proc rand_crc
  lda #$00
.endproc
.proc crc16_update
        EOR CRCHI       ; A contained the data
        STA CRCHI       ; XOR it into high byte
        LSR             ; right shift A 4 bits
        LSR             ; to make top of x^12 term
        LSR             ; ($1...)
        LSR
        TAX             ; save it
        ASL             ; then make top of x^5 term
        EOR CRCLO       ; and XOR that with low byte
        STA CRCLO       ; and save
        TXA             ; restore partial term
        EOR CRCHI       ; and update high byte
        STA CRCHI       ; and save
        ASL             ; left shift three
        ASL             ; the rest of the terms
        ASL             ; have feedback from x^12
        TAX             ; save bottom of x^12
        ASL             ; left shift two more
        ASL             ; watch the carry flag
        EOR CRCHI       ; bottom of x^5 ($..2.)
        STA CRCHI       ; save high byte
        TXA             ; fetch temp value
        ROL             ; bottom of x^12, middle of x^5!
        EOR CRCLO       ; finally update low byte
        LDX CRCHI       ; then swap high and low bytes
        STA CRCHI
        STX CRCLO
        RTS
.endproc

;;
; Sets up the initial LRU queues for both players.
.proc lru4_init
  ldx #NUM_PIECES * 2 - 1
loop:
  txa
  lsr a
  sta lru_queue,x
  sta lru_queue+2*REPEATED_PIECES,x
  dex
  bpl loop
  sta player_dealdepth+0
  sta player_dealdepth+1
  rts
.endproc

;;
; Deals pieces by randomly choosing one of the four frontmost
; pieces and rotates pieces later in the queue forward by one.
; Over time, more complex pieces become added to the rotation.
.proc lru4_deal_piece
chosen_next = 0
end_of_rotation = 1
  jsr rand_crc
  and #$06
  ora cur_turn
  tay
  lda lru_queue,y
  sta chosen_next
  ldx cur_turn
  lda player_dealdepth,x
  .repeat ::DEALDEPTH_SHIFT
    lsr a
  .endrepeat
  asl a
  adc #(MIN_PIECES + REPEATED_PIECES - 1) * 2
  sta end_of_rotation

  lda player_dealdepth,x
  cmp #(NUM_PIECES - MIN_PIECES) << DEALDEPTH_SHIFT
  bcs :+
  inc player_dealdepth,x
:
rotate_loop:
  lda lru_queue+2,y
  sta lru_queue,y
  iny
  iny
  cpy end_of_rotation
  bcc rotate_loop
  lda chosen_next
  sta lru_queue,y
  rts
.endproc
