;
; RHDE battle: main loop and unit control
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

MAX_UNIT_HP = 3
BATRADIUS = 2
PHASE_SKIP_TIME = 60  ; hold B on both controllers this long to skip phase

.segment "ZEROPAGE"
b_hold_time:     .res 2

unitzp24:
unit_x:          .res 2 * MAX_UNITS
unit_y:          .res 2 * MAX_UNITS
unit_state:      .res 2 * MAX_UNITS  ; 7: inactive; 5-4: dir; 3-0: frame
unit_state_time: .res 2 * MAX_UNITS
unit_hp:         .res 2 * MAX_UNITS
unit_move_vector:.res 2 * MAX_UNITS  ; XXXXYYYY, each from -7 to 7
unit_carry_item: .res 2 * MAX_UNITS  ; 7: empty; 6-1: item id; 0: player
player_cannon_x: .res 2  ; coords of cannon that current unit faces,
player_cannon_y: .res 2  ; or <0 if none
player_particle_x: .res 2
player_particle_y: .res 2

facing_furni_x:  .res 1
facing_furni_y:  .res 1
facing_furni_id: .res 1

player_cur_unit = cur_piece_lo
player_state = cur_piece_hi
last_frame_keys = next_piece

; compo version had 50 seconds which was barely enough for 1 1/2 laps
; 2024 suite version is being tuned
COMBAT_PHASE_DURATION = 75

; Behavior at end of battle phase
; 0: You keep furni that you pick up; possession is 90% of law
; 1: Return taken item to owner if you don't reach your base in time
; NESdev Compo 2014 version had 0
RETURN_FURNI_TO_OWNER = 0

UNIT_STATE_STAND     = $00
UNIT_STATE_WALK      = $01
UNIT_STATE_SWING_BAT = $04
UNIT_STATE_STUNNED   = $06
UNIT_STATE_FRAME     = $0F
UNIT_STATE_DIR       = $30

.segment "CODE"
.proc battle_phase
  jsr init_cannons
  jsr init_dropped_furni
  jsr init_breakdoors
  lda #$FF
  sta cursor_x+0
  sta cursor_x+1

  ; Reset all units
  lda #$01
  sta player_cur_unit+1
  lsr a
  sta player_cur_unit+0
  sta player_state+0
  sta player_state+1
  sta time_tenths
  sta time_subtenths
  lda #COMBAT_PHASE_DURATION
  sta phase_seconds
  ldx #2 * MAX_UNITS - 1
  unitloop:
    lda #$00
    sta unit_move_vector,x
    sta unit_state_time,x
    lda #MAX_UNIT_HP
    sta unit_hp,x
    lda #$FF
    sta unit_state,x
    sta unit_carry_item,x
    dex
    bpl unitloop

  ; 2014-06-27: Skip the round if no beds are present
  jsr count_beds
  lda $00
  ora $01
  bne mainloop
  rts

mainloop:

  ; last_frame_keys logic
  ; If a key is pressed, it goes high in last_frame_keys.
  ; If a key is not being held, it goes low in last_frame_keys.
  ; To check if a key is released, check
  ; (~cur_keys) & last_frame_keys.
  ; To require that a key be repressed before releases are
  ; recognized, set it to 0 in last_frame_keys.  For example, B+A
  ; to switch units would require that B be repressed so that
  ; the newly selected unit doesn't drop his carried item.
  lda cur_keys+0
  and last_frame_keys
  ora new_keys+0
  sta last_frame_keys
  lda cur_keys+1
  and last_frame_keys+1
  ora new_keys+1
  sta last_frame_keys+1
  jsr read_pads
  jsr pently_update

  ; once the timer hits 0, freeze controls and let missiles in
  ; flight reach their targets
  jsr phase_skip_check
  lda phase_seconds
  beq no_control
    ldx #0
    jsr control_player
    ldx #1
    jsr control_player
  no_control:

  jsr move_one_unit
  lda time_subtenths  ; move missiles on odd frames
  lsr a
  bcs :+
    jsr move_cannon_missiles
  :

  ; and with everything moved into position, draw everything
  lda #0
  sta oam_used
  jsr build_draw_timer
  jsr draw_cursors
  jsr draw_unit_carry_items
  jsr draw_unit_furni_masks
  jsr draw_cannon_missiles
  jsr draw_unit_weapons
  jsr draw_units
  jsr draw_dropped_furni
  jsr draw_phase_skip_check
  jsr draw_debughex
  ldx oam_used
  jsr ppu_clear_oam
  jsr build_choose_redraw
  jsr bgup_vsync

.if 0
  lda new_keys+0
  and #KEY_START
  beq mainloop  
.else
  jsr countdown_logic
  bcs not_54321
  lda #SFX_COUNTDOWN
  jsr pently_start_sound
not_54321:
  lda phase_seconds
  bne mainloop
  jsr pently_stop_music
  jsr any_cannon_missiles_left
  bpl mainloop
.endif

  ; Return picked up items to their owner or possessor
  ldy #2 * MAX_UNITS - 1
returnloop:
  ldx unit_carry_item,y
  cpx #NUM_FURNIS * 2
  bcs :+
.if ::RETURN_FURNI_TO_OWNER = 0
  tya
  lsr a  ; C = player who possesses item
  txa
  and #%11111110
  adc #$00
  tax
.endif
  inc player_inv,x
:
  dey
  bpl returnloop
  jmp cleanup_dropped_furni
.endproc

;;
; If both players want to proceed to the next phase, have both hold B
.proc phase_skip_check
  ldx #1
  countloop:
    inc b_hold_time,x
    bne :+
      dec b_hold_time,x
    :
    lda cur_keys,x
    and #KEY_B
    bne :+
      sta b_hold_time,x
    :
    dex
    bpl countloop

  lda #PHASE_SKIP_TIME
  cmp b_hold_time+0
  bcs nope
  cmp b_hold_time+1
  bcs nope
    lda #0
    sta phase_seconds
  nope:
  rts
.endproc

;;
; Counts beds belonging to each player, starting from the center
; rows out, and places a unit at the head of the first MAX_UNITS
; beds on each side.
.proc count_beds
beds0 = $00
beds1 = $01
rownum = $02
roadx = $03
  lda #0
  sta beds0
  sta beds1
  lda #FIELD_HT/2-1
rowloop:
  jsr do1row
  lda #FIELD_HT-1
  sbc rownum
  jsr do1row
  lda #FIELD_HT-2
  sbc rownum
  bcs rowloop
  rts

; in: A = row number
; out: row number stored in rownum, carry set
do1row:
  sta rownum
  jsr seek_fielddata_row_a
  lda rownum
  lsr a
  tax
  lda xmax_1p,x
  asl a
  sta roadx
  ldy #28
tileloop:
  lda (fieldlo),y
  cmp #TILE_BED
  bne notbed
  ldx #0
  cpy roadx
  bcc is_west_of_road
  inx
is_west_of_road:
  lda beds0,x
  inc beds0,x
  
  ; If the team isn't already full, add one unit
  rol a
  bcs notbed
  cmp #2*MAX_UNITS
  bcs notbed
  tax
  lda #UNIT_STATE_STAND
  sta unit_state,x
  tya
  asl a
  asl a
  asl a
  sta unit_x,x
  lda rownum
  asl a
  asl a
  asl a
  sta unit_y,x
notbed:
  dey
  bne tileloop
  sec
  rts
.endproc

.proc control_player
  stx cur_turn
  lda #$FF
  sta player_particle_y,x
  ldy player_state,x
  lda handlers+1,y
  pha
  lda handlers,y
  pha
  rts
.pushseg
.segment "RODATA"
handlers:
  .addr control_unit-1, control_cannon-1
.popseg
.endproc

;;
; User control of the units.
; X = player number
.proc control_unit
xdist = 4
ydist = 5

  jsr furni_in_front_of_unit
  jsr is_unit_by_cannon
  ldx cur_turn
  sta player_cannon_y,x
  sty player_cannon_x,x

  ; A is start cannon; B+A is tag out
  lda cur_keys,x
  asl a
  bmi not_control_cannon
  tya                   ; A.D7 = 0 for facing, 1 for not
  ora cur_keys,x
  eor #$80              ; A.D7 = 1 for facing and not holding A
  and last_frame_keys,x ; A.D7 = 1 for facing and releasing A
  bpl not_control_cannon
    ; The player wants to take control of the cannons.
    ; If the cannons aren't aimed (cursor_x > RIGHTMOST_X),
    ; move the cursor to the cannon's position.
    lda cursor_x,x
    cmp #RIGHTMOST_X+1
    bcc cannon_cursor_not_set
      lda player_cannon_x,x
      sta cursor_x,x
      lda player_cannon_y,x
      sta cursor_y,x
    cannon_cursor_not_set:
    lda #PLAYER_CONTROL_CANNON
    sta player_state,x
    rts
  not_control_cannon:

  ; Release A or B while standing: Do something
  lda cur_keys,x
  eor #$FF
  and last_frame_keys,x
  and #KEY_B|KEY_A
  bpl not_use_weapon
  
  ; Release A: If the player is carrying a weapon, use it
  lda player_cur_unit,x
  tax
  lda unit_state,x
  and #$0F  ; if not stopped, control moveq
  bne control_unit_x_moveq
  lda unit_carry_item,x
  lsr a
  cmp #item_bat
  bne control_unit_x_moveq
  ; Wind up for a swing
  lda #2
  sta unit_state_time,x
  lda unit_state,x
  and #UNIT_STATE_DIR
  ora #UNIT_STATE_SWING_BAT
  sta unit_state,x
notSwitchUnits:
  rts
not_use_weapon:

  ; Release A or B: pick up indoor furni
  beq not_pickup
  jsr pickup_indoor_facing_furni
not_pickup:

  lda player_cur_unit,x
  tax
control_unit_x_moveq:
  jsr get_unit_step_dest
  jsr unit_get_move_vector
  
  ; 2014-03-12: make unit movement autorepeat
  txa
  pha
  and #1
  tay   ; top of stack = unit number, Y = team number
  tax
  lda #KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT
  and das_keys,x
  sta das_keys,x
  jsr autorepeat
  pla
  tax   ; X = unit number, Y = team number

  lda new_keys,y
  lsr a
  bcc notRight
    inc xdist
    ldy #$00
    bpl have_pressed_dir
  notRight:

  lsr a
  bcc notLeft
    dec xdist
    ldy #$20
    bpl have_pressed_dir
  notLeft:

  lsr a
  bcc notDown
    inc ydist
    ldy #$10
    jmp have_pressed_dir
  notDown:

  lsr a
  bcc notUp
    dec ydist
    ldy #$30
  have_pressed_dir:

  ; If unit X is stopped, and moveq was empty before this press,
  ; face direction Y
  lda unit_move_vector,x
  bne no_turn_before_pack
  lda unit_state,x
  and #UNIT_STATE_FRAME
  bne no_turn_before_pack
    tya
    sta unit_state,x
  no_turn_before_pack:
  jmp unit_set_move_vector_if_passable
notUp:

  ; B+A or Select: Switch units
  lda new_keys,y  ; is B, A, or Select pressed?
  and #KEY_B|KEY_A|KEY_SELECT
  beq notSwitchUnits
  and #KEY_SELECT
  bne tag_out
  lda cur_keys,y  ; If Select not pressed, are B and A held?
  cmp #KEY_B|KEY_A
  bcc notSwitchUnits
  ; fall through to tag_out
.endproc
.proc tag_out

  ; Require buttons to be pressed again before releases are
  ; recognized.
  ldy cur_turn
  lda #0
  sta new_keys,y
  sta last_frame_keys,y

  ; Have to store the value at player_cur_unit,y in a
  ; local variable because you can't CPX a,Y on 6502.
player_y_cur_unit = $02
  ldx player_cur_unit,y
  stx player_y_cur_unit

  ; Find a unit to switch to, the next in rotation that exists
  ; and is not stunned
  find_next_unit:
    inx
    inx
    cpx #2 * MAX_UNITS
    bcc switchNotWrapEnd
      txa
      and #$01
      tax
    switchNotWrapEnd:
    cpx player_y_cur_unit
    beq notSwitchUnits
    lda unit_state,x
    bmi find_next_unit
    and #$0F
    cmp #UNIT_STATE_STUNNED
    beq find_next_unit
  txa  ; XXX: Redundant check?
  cmp player_cur_unit,y
  bne haveNewUnit
  notSwitchUnits:
    rts
  haveNewUnit:
  sta player_cur_unit,y
  lda #0
  sta player_state,y

  ; If this unit is a cannon, and the cannon has been turned on
  ; for the first time, log in!
  lda cursor_x,y
  bmi notEnterCannon
  tya
  tax
  jsr furni_in_front_of_unit
  jsr is_unit_by_cannon
  bmi notEnterCannon
    lda #PLAYER_CONTROL_CANNON
    sta player_state,y
  notEnterCannon:
  lda #SFX_TURN_PAGE
  jmp pently_start_sound
.endproc

;;
; @param Y unit number
; @return $00: x, $01: y, $07: unit number
.proc query_dropped_furni_unit_y
xsave = $00
ysave = $01
unitnum = $07
  ;If unit is carrying something
  sty unitnum
  lda unit_x,y
  lsr a
  lsr a
  lsr a
  tax
  lda unit_y,y
  lsr a
  lsr a
  lsr a
  tay
  jmp query_dropped_furni  
.endproc

;;
; Unit Y drops furni
.proc drop_furni
xsave = $00
ysave = $01
unitnum = $07

  jsr query_dropped_furni_unit_y
  beq pickup_done  ; if there's something already there, don't drop
  ldy unitnum
  lda unit_carry_item,y
  ldx xsave
  ldy ysave
  jsr add_dropped_furni
  ldy unitnum
  lda #$FF
  sta unit_carry_item,y
  lda #SFX_PLACE
  jsr pently_start_sound
pickup_done:
  lda unitnum
  and #$01
  tax
  rts
.endproc

;;
; B: Pick up or drop furniture
.proc pickup_indoor_facing_furni
itemdata = $00
itemoffset = $02

  ; Make sure unit isn't already carrying something
  ldy player_cur_unit,x
  lda unit_carry_item,y
  bpl drop_furni

  ; Is unit on top of a leaf?
  jsr query_dropped_furni_unit_y
  bne no_dropped_furni
  jsr take_dropped_furni
  ldy $07
  sta unit_carry_item,y
  lda #SFX_TURN
  jsr pently_start_sound
  ldy $07
  tya
  and #$01
  tax
  rts

no_dropped_furni:
  ldy $07
  tya
  and #$01
  tax

  ; Find the furni's entry (and the main rotation while we're at it)
  lda facing_furni_id
  jsr find_furni_main_rot
  bcs not_pickup
  sta itemoffset
  
  ; Make sure it's indoor
  jsr seek_to_furni_a
  ldy #SHOPITEMS_SIZE
  lda #SHOPITEMS_INDOOR
  and (itemdata),y
  beq not_pickup
  
  ; Find which side the furni is on
  ; 7: 0 for carrying something
  ; 6-1: item ID
  ; 0: Side from which it originated
  lda facing_furni_y
  lsr a
  tay
  lda facing_furni_x
  lsr a
  cmp xmax_1p,y
  rol itemoffset
  
  ; See if the unit can pick up this item at all.  Don't let a unit
  ; pick up its own furniture unless it's a weapon.
  lda itemoffset
  cmp #NUM_FURNIS_NONWEAPON * 2
  bcs is_weapon_or_opponents
  txa
  eor itemoffset
  lsr a
  bcc not_pickup
is_weapon_or_opponents:
  
  ; Set the unit as carrying this item
  ldy player_cur_unit,x
  lda itemoffset
  sta unit_carry_item,y

  ; And erase the furni in its current rotation
  lda facing_furni_x
  sta cursor_x,x
  lda facing_furni_y
  sta cursor_y,x
  lda facing_furni_id
  jsr seek_to_furni_a
  ldy #SHOPITEMS_SIZE
  lda (itemdata),y
  jsr erase_furni

  lda #0
  jmp pently_start_sound
not_pickup:
  rts
.endproc

;;
; Choose one of the units (based on game time) and move it.
.proc move_one_unit
  ; Move one unit per frame, and handle cannons in other frames
  ; (each unit gets 5 moving turns per second)
  lda time_tenths
  lsr a
  lda time_subtenths
  rol a
  cmp #2 * (2 + MAX_UNITS)
  bcs bail_1
  cmp #2 * MAX_UNITS
  bcc not_cannon_cooldown
  lsr a
  jmp cannon_cooldown
not_cannon_cooldown:
  tax
  lda unit_state,x
  bpl not_inactive
bail_1:
  rts
not_inactive:

  ; At this point we know what active unit to move
  and #$0F
  bne not_stand
  
  ; Drop off carried furni if on home territory
sidetmp = $07  ; sound modifies $00-$04, so stay out of its way
  lda unit_carry_item,x
  cmp #NUM_FURNIS_NONWEAPON*2
  bcs no_dropoff
  lda unit_y,x
  lsr a
  lsr a
  lsr a
  lsr a
  tay
  lda unit_x,x
  lsr a
  lsr a
  lsr a
  lsr a
  cmp xmax_1p,y
  rol sidetmp
  cmp xmin_2p,y
  lda sidetmp
  and #$01
  rol a  ; 0: left side; 3: right side; 2: on road
  cmp #2
  beq no_dropoff
  txa
  eor sidetmp
  lsr a  ; C = 0 for same side, 1 for different side
  bcs no_dropoff

  ; Move item from unit to stock
  lda unit_carry_item,x
  eor sidetmp
  and #%11111110
  eor sidetmp
  stx sidetmp
  tax
  inc player_inv,x
  lda #SFX_ENCLOSE
  jsr pently_start_sound
  ldx sidetmp
  lda #$FF
  sta unit_carry_item,x
no_dropoff:

  ; Decide where to walk
  jsr decide_moveq_direction
  tya
  bpl autowalk_proceed
  ; If the overly simplistic approach to pathfinding ends up
  ; stuck in a corner, clear movement vector
  lda #0
  sta unit_move_vector,x
  rts
autowalk_proceed:
  sta unit_state,x

  ; Subtract 1 pace from the unit's moveq
  lsr a
  lsr a
  lsr a
  lsr a
  tay  ; Y = direction
  jsr unit_get_move_vector
  lda 4
  sec
  sbc direction_xstep,y
  sta 4
  lda 5
  sec
  sbc direction_ystep,y
  sta 5
  jsr unit_set_move_vector
  jmp take_one_step
not_stand:

  cmp #UNIT_STATE_SWING_BAT
  bcc take_one_step
  ; All states higher than walking have a delay time
  dec unit_state_time,x
  bne state_time_not_yet

  ; Currently, all states other than the first state of
  ; swinging the bat just go to standing afterward unless
  ; HP is 0
  cmp #UNIT_STATE_SWING_BAT
  beq swinging_bat
  eor unit_state,x
  ldy unit_hp,x
  bne :+
  lda #$FF
:
  sta unit_state,x
state_time_not_yet:
  rts
  
take_one_step:
  lda unit_state,x
  lsr a
  lsr a
  lsr a
  lsr a
  tay
  lda direction_xstep,y
  asl a
  clc
  adc unit_x,x
  sta unit_x,x
  lda direction_ystep,y
  asl a
  clc
  adc unit_y,x
  sta unit_y,x
  inc unit_state,x
  lda unit_state,x
  and #$03
  bne bail_2

  ; Stop walking
  lda unit_state,x
  and #UNIT_STATE_DIR
  .if ::UNIT_STATE_STAND
    ora #UNIT_STATE_STAND
  .endif
  sta unit_state,x
  
  ; Snap position to 8x8 grid
  ; TO DO
  lda unit_x,x
  clc
  adc #4
  and #<-8
  sta unit_x,x
  lda unit_y,x
  clc
  adc #4
  and #<-8
  sta unit_y,x
bail_2:
  rts
.endproc

;;
; Handle contact frame of bat
.proc swinging_bat
xtile = 2
ytile = 3
unitnum = 4  ; need to save it for droppedfurni
numhit = 5
xpx = 6
ypx = 7
  inc unit_state,x
  lda #0
  sta numhit
  lda #3
  sta unit_state_time,x

  ; Calculate the top left corner of the bat's hitbox
  ; and the tile over which the hitbox is centered
  lda unit_state,x
  lsr a
  lsr a
  lsr a
  lsr a
  tay
  lda unit_x,x
  stx unitnum
  clc
  adc direction_batrectleft,y
  sta xpx
  clc
  adc #BATRADIUS + 4
  lsr a
  lsr a
  lsr a
  sta xtile
  lda unit_y,x
  clc
  adc direction_batrecttop,y
  sta ypx
  clc
  adc #BATRADIUS + 4
  lsr a
  lsr a
  lsr a
  sta ytile
  
  ; Check bat against destructible teampass tiles (doors and fences)
  cmp #FIELD_HT
  bcs not_door
  jsr seek_fielddata_row_a
  ldy xtile
  lda (fieldlo),y
  cmp #TILE_FENCE
  beq is_door
  and #$FE
  cmp #CONNBLKS_DOORWE
  bne not_door
is_door:
  inc numhit
  ldx xtile
  ldy ytile
  jsr inc_breakdoors
  bcc not_door
  ldy xtile
  lda #TILE_RUBBLE
  sta (fieldlo),y
  jsr update_around_column_y
not_door:

  ; Loop over opponent units
  lda unitnum
  and #$01
  eor #(2 * MAX_UNITS - 1)
  tax
opponentloop:
  lda unit_state,x
  bmi no_opponent
  and #$0F
  cmp #UNIT_STATE_STUNNED
  beq no_opponent
  lda unit_x,x
  sec
  sbc xpx
  sta debughex+0
  cmp #BATRADIUS*2+1
  bcs no_opponent
  lda unit_y,x
  sec
  sbc ypx
  sta debughex+1
  cmp #BATRADIUS*2+1
  bcs no_opponent

  ; presence, mercy, and hitbox tests pass: stun the opponent
  inc numhit
  lda unit_state,x
  and #UNIT_STATE_DIR
  ora #UNIT_STATE_STUNNED
  sta unit_state,x
  lda #10  ; stun time in 20 ms
  sta unit_state_time,x
  lda unit_hp,x
  beq :+
  dec unit_hp,x
:

  ; Drop carried item if either 0 HP or not a weapon
  lda unit_carry_item,x
  bmi no_opponent
  cmp #NUM_FURNIS_NONWEAPON * 2
  bcc dropping_nonweapon
  lda unit_hp,x    ; if it's a weapon, drop it only when fainting
  bne no_opponent
dropping_nonweapon:

  ; Drop the item
  stx unitnum
  lda unit_carry_item,x
  pha
  lda #$FF
  sta unit_carry_item,x

  ; Find on which tile the item lands
  ldy ytile
  ldx xtile
  pla
  jsr add_dropped_furni
  ldx unitnum
no_opponent:
  dex
  dex
  bpl opponentloop
  lda numhit
  beq nonehit
  lda #SFX_PLACE
  jsr pently_start_sound
nonehit:
  lda #SFX_TURN_PAGE
  jmp pently_start_sound
  rts
.endproc

.segment "RODATA"
; 0 right, 1 down, 2 left, 3 up
; lots of overlaps are done to share zeroes
direction_ystep: .byte 0
direction_xstep: .byte 1, 0, <-1
direction_batoffsety: .byte 0
direction_batoffsetx: .byte 5, 0, <-5
direction_hflip: .byte $00, $00, $40, $00
direction_tilerow: .byte $10, $20, $10, $00
direction_batrecttop:
  .byte <(0 - BATRADIUS)
direction_batrectleft:
  .byte <(8 - BATRADIUS), <(0 - BATRADIUS), <(-8 - BATRADIUS)
  .byte <(0 - BATRADIUS)
player_sprite_origin: .byte $A0, $D0
.segment "CODE"

;;
; Finds where a unit is and where it's facing.
; @param X the unit to test
; @return 0: x tile; 1: y tile; Y: index into direction_*step
.proc get_unit_pos_dir
dst_x = 0
dst_y = 1
  lda unit_x,x
  sta dst_x
  lda unit_y,x
  sta dst_y
  lda unit_state,x
  lsr a
  lsr a
  lsr a
  lsr a
  tay
  rts
.endproc

;;
; Finds where a unit will be at the end of this step.
; @param X the unit to test
; @return 0: x tile; 1: y tile
.proc get_unit_step_dest
dst_x = 0
dst_y = 1
xoffs = 2
yoffs = 3
  jsr get_unit_pos_dir
  lda direction_xstep,y
  asl a
  sta xoffs
  lda direction_ystep,y
  asl a
  sta yoffs
  lda #4
  sec
  sbc unit_state,x
  and #$03
  beq noframesleft
  tay
framesleftloop:
  lda dst_x
  clc
  adc xoffs
  sta dst_x
  lda dst_y
  clc
  adc yoffs
  sta dst_y  
  dey
  bne framesleftloop
noframesleft:
  lda dst_x
  lsr a
  lsr a
  lsr a
  sta dst_x
  lda dst_y
  lsr a
  lsr a
  lsr a
  sta dst_y
  rts
.endproc

;;
; Unpacks the distance that a unit needs to move.
; @param X the unit to test
; @return 4, 5: move queue distance
.proc unit_get_move_vector
  lda unit_move_vector,x
.endproc
.proc unpack_x4y4_signed
dist_x = $04
dist_y = $05
  pha
  lsr a
  lsr a
  lsr a
  lsr a
  eor #$F8
  clc
  adc #$08
  sta dist_x
  pla
  and #$0F
  eor #$F8
  clc
  adc #$08
  sta dist_y
  rts
.endproc

;;
; Determines whether a particular tile number is passable.
.proc is_tile_passable
tileid = 6
  cmp #TILE_FENCE
  beq ret_teampass
  lsr a
  eor #TILE_OV_DOOR_WE >> 1
  beq ret_teampass
  eor #(CONNBLKS_DOORWE ^ TILE_OV_DOOR_WE) >> 1
  bne not_teampass
ret_teampass:
  lda #PASSABILITY_TEAM
  rts
not_teampass:
  eor #CONNBLKS_DOORWE >> 1
  rol a
  sta tileid
  lsr a
  lsr a
  lsr a
  tay
  lda tileid
  and #$07
  sta tileid
  lda tile_pass_table,y
  ldy tileid
  and lsr_masks,y
  beq is_passable
  lda #PASSABILITY_NO
is_passable:
  rts
.endproc

.segment "RODATA"
tile_pass_table:
  ; 1 for each solid tile
  .dbyt %0000000000000000
  .dbyt %1100000000000000
  .dbyt %1111110000000000
  .dbyt %1111110000000000
  .dbyt %1111111111111111
  .dbyt %1100000011110011
  .dbyt %0000000000111111
  .dbyt %1111111110111111
lsr_masks:
  .repeat 8, I
    .byte $80 >> I
  .endrepeat

.segment "CODE"


;;
; Decides whether a tile at a given position is passable for the unit
; on a given team.
; @param A tile number
; @param X player or unit number
; @param 0 X tile
; @param 1 Y tile position
; @return A = 0 if passable, 1 if not, 2 if wrong team; ZF reflects A
.proc is_tile_passable_team_x
xtile = 0
ytile = 1
  jsr is_tile_passable
  cmp #PASSABILITY_TEAM
  bne have_a
  lda ytile
  lsr a
  tay  ; Y = distance in attribute spaces from top of map
  lda xtile
  lsr a
  adc #0         ; A = (X / 2) rounded up
  cmp xmin_2p,y  ; C = 0 for L team's territory, 1 for R team's
  txa            ; A.D0 = what team the unit is on
  adc #0         ; A.D0 = 1 if the teams differ
  and #$01       ; A = 0 for match or 1 for mismatch
  asl a
have_a:
  cmp #0
  rts
.endproc

.proc decide_moveq_direction
pt_x = 0
pt_y = 1
mv_x = 4
mv_y = 5
  jsr get_unit_step_dest
  jsr unit_get_move_vector
  lda mv_x
  beq have_mv_x
  jsr tile_to_addr
  lda mv_x
  bmi move_x_neg
  iny
  jmp move_x_have_y
move_x_neg:
  dey
move_x_have_y:
  ; See if we can move unit X horizontally to column Y.
  lda (fieldlo),y
  jsr is_tile_passable_team_x
  bne stop_x
  lda #$80
  bit mv_x
  bpl have_mv_x
  lda #$A0
  bne have_mv_x
stop_x:
  lda #0
have_mv_x:
  eor #$80
  sta mv_x

  lda mv_y
  beq have_mv_y
  bmi move_y_neg
  inc pt_y
  jmp move_y_have_row
move_y_neg:
  dec pt_y
move_y_have_row:
  ; See if we can move unit X horizontally to row Y.
  jsr tile_to_addr
  bcs stop_y
  lda (fieldlo),y
  jsr is_tile_passable_team_x
  bne stop_y
  lda #$90
  bit mv_y
  bpl have_mv_y
  lda #$B0
  bne have_mv_y
stop_y:
  lda #0
have_mv_y:
  eor #$80
  
  ; mv_x is the horizontal move direction
  ; (00: right, 20: left, 80: stay put)
  ; mv_y is the vertical move direction
  ; (10: down, 30: up, 80: stay put)
  ; If there's no vertical move direction, use the horizontal one
  bpl :+
  lda mv_x
:
  sta mv_y
  ; If there's no horizontal move direction, use the vertical one
  bit mv_x
  bpl :+
  sta mv_x
:

  ; If the unit is facing L/R, prefer U/D; otherwise prefer L/R
  ldy mv_y
  lda unit_state,x
  and #$10
  beq :+
  ldy mv_x
:
  rts
.endproc

MOVE_VECTOR_MAX = 7

;;
; Sets the unit's movement vector if the destination is not solid.
; @param $0004 horizontal offset
; @param $0005 vertical offset
.proc unit_set_move_vector_if_passable
xdist = $04
ydist = $05
step_dst_x = $00
step_dst_y = $01
  jsr get_unit_step_dest
  lda ydist
  cmp #$100 - MOVE_VECTOR_MAX
  bcs ydist_in_lr_range
  cmp #MOVE_VECTOR_MAX+1
  bcs out_of_range
ydist_in_lr_range:
  clc
  adc step_dst_y
  cmp #FIELD_HT
  bcs out_of_range
  sta step_dst_y
  jsr seek_fielddata_row_a

  lda xdist
  cmp #$100 - MOVE_VECTOR_MAX
  bcs xdist_in_lr_range
  cmp #MOVE_VECTOR_MAX+1
  bcs out_of_range
xdist_in_lr_range:
  clc
  adc step_dst_x
  cmp #LEFTMOST_X
  bcc out_of_range
  cmp #RIGHTMOST_X + 1
  bcs out_of_range
  sta step_dst_x

  tay
  lda (fieldlo),y
  jsr is_tile_passable_team_x
  beq unit_set_move_vector
out_of_range:
  sec
  rts
.endproc
.proc unit_set_move_vector
xdist = $04
ydist = $05
  lda xdist
  asl a
  asl a
  asl a
  asl a
  eor ydist
  and #$F0
  eor ydist
  sta unit_move_vector,x
  rts
.endproc


;;
; Seeks to the tile in front of the player's (stopped) active unit.
; @param X player number (0 or 1)
; @return CF=0 if valid (unit is stopped and tile is in range);
; X = ID of active unit; Y = dst_x = x coord; dst_y = y coord;
; fieldlo = start of row dst_y
.proc tile_in_front_of_unit
dst_x = 0
dst_y = 1
  lda player_cur_unit,x
  tax
  lda unit_state,x
  and #$0F
  ora unit_move_vector,x
  beq is_stopped
  sec
  rts
is_stopped:
  ; Add one step forward. If Y out of bounds, fail.
  jsr get_unit_pos_dir
  lda dst_x
  lsr a
  lsr a
  lsr a
  adc direction_xstep,y
  sta dst_x
  lda dst_y
  lsr a
  lsr a
  lsr a
  adc direction_ystep,y
  sta dst_y
  jmp tile_to_addr
.endproc

.proc tile_under_unit_x
dst_x = 0
dst_y = 1
  lda unit_x,x
  lsr a
  lsr a
  lsr a
  sta dst_x
  lda unit_y,x
  lsr a
  lsr a
  lsr a
  sta dst_y
  jmp tile_to_addr
.endproc

;;
; Finds the position of the top left corner of the furniture object
; in front of player X's active unit and stores it in facing_furni_*.
; @return A: fieldlo; Y: furni id (not necessarily main rot);
; flags: BCC if Y less than NUM_ALL_FURNIS;
; facing_furni_x: left of furni if valid furni else X in front;
; facing_furni_y: top of furni if valid furni else Y in front;
; fieldlo: row of tile in front; X = active unit
.proc furni_in_front_of_unit
  jsr tile_in_front_of_unit
  bcc inbounds
  ldy #$FF
  sty facing_furni_x
  sty facing_furni_y
  sty facing_furni_id
  rts
inbounds:
  sty facing_furni_x
  lda 1
  sta facing_furni_y
  lda (fieldlo),y
  
  ; If facing a tile that obviously isn't furniture (floor, rubble,
  ; wall, door), look at the tile under the player instead
  cmp #TILE_FLOOR
  beq is_facing_floor
  cmp #TILE_RUBBLE
  beq is_facing_floor
  cmp #CONNBLKS_ROW
  bcc not_facing_floor
  cmp #CONNBLKS_DOORWE
  bcs not_facing_floor
is_facing_floor:
  jsr tile_under_unit_x
  sty facing_furni_x
  lda 1
  sta facing_furni_y
  lda (fieldlo),y
not_facing_floor:
  jsr find_furni_tile_a
  ; Y=furni id; A=offset from base YYYYXXXX
  bpl is_a_furni

  ; Tile is not a furni.  If it is the opponent's team-passable tile
  ; (door or fence), draw a lock icon.
  ldy facing_furni_x
  sty 0
  lda facing_furni_y
  sta 1
  lda (fieldlo),y
  jsr is_tile_passable_team_x
  cmp #PASSABILITY_TEAM
  bne not_teamblocked
  ; if teamblocked, place a lock sign
  txa
  and #$01
  tay
  lda facing_furni_x
  sta player_particle_x,y
  lda facing_furni_y
  sta player_particle_y,y
not_teamblocked:
  ldy #$FF
  bmi have_furni_id
is_a_furni:
  cmp #$10
  bcc :+
  and #$0F
  dec facing_furni_y
:
  cmp #$01
  bcc :+
  dec facing_furni_x
:
have_furni_id:
  sty facing_furni_id
  cpy #NUM_ALL_FURNIS
  rts
.endproc

;;
; Finds the location of the cannon that a player's active unit
; is facing.
; @param X the unit to test
;        facing_furni_*: set by furni_in_front_of_unit
; @return Y: x tile (or >=128 if none); A, fieldlo: y tile;
; NF true if nonexistent
.proc is_unit_by_cannon
  lda unit_carry_item,x  ; Cannot operate cannon while holding item
  bpl nope1
  ldy facing_furni_id
  cpy #NUM_ALL_FURNIS
  bcc is_inbounds
nope1:
  ldy #$FF
  tya
  rts
is_inbounds:

  ; If facing something other than a silo, fail
  cpy #item_silo
  beq is_correct_item
  cpy #item_siloused
  bne nope1
is_correct_item:
  lda facing_furni_y
  ; If not on own side, fail
  lsr a
  tay
  lda facing_furni_x
  lsr a
  cmp xmax_1p,y
  txa  ; C=side (0=left, 1=right); A=player (0=left, 1=right)
  adc #$00  ; A.0=player (0=same, 1=different)
  lsr a
  bcs nope1

  lda facing_furni_y
  ldy facing_furni_x  
  rts
.endproc

; PRESENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc draw_cursors
  lda nmis
  jsr draw_one_cursor
  lda nmis
  eor #$01

; Draw cursor for player A&1
draw_one_cursor:
  and #$01
  tax
  lda player_state,x
  beq player_is_walking
    jmp draw_cannon_cursor
  player_is_walking:

  ldy oam_used
  ; If facing a locked door (player_particle_y < 0x80),
  ; draw the lock
  lda player_particle_y,x
  bmi no_lock_particle
    asl a
    asl a
    asl a
    clc
    adc #FIELDTOP_Y - 1
    sta OAM+0,y
    lda player_particle_x,x
    asl a
    asl a
    asl a
    clc
    adc #FIELDTOP_X - 1
    sta OAM+3,y
    txa
    eor #$01
    sta OAM+2,y
    lda #TILE_LOCK_POPUP
    sta OAM+1,y
    iny
    iny
    iny
    iny
  no_lock_particle:

  ; If this unit exists and is not stunned, draw a "player 1" or
  ; "player 2" arrow marker above the unit
  lda player_cur_unit,x
  tax
  lda unit_state,x    ; Nonexistent unit: don't draw marker
  bmi noplayer
  and #$0F
  cmp #UNIT_STATE_STUNNED  ; Stunned unit: don't draw marker
  beq noplayer
    lda unit_x,x
    clc
    adc #FIELDTOP_X
    sta OAM+3,y
    lda unit_y,x
    clc
    adc #FIELDTOP_Y - 1 - 1 - 9
    sta OAM+0,y
    txa
    and #$01
    sta OAM+2,y
    ora #TILE_PLAYER_MARKER
    sta OAM+1,y
    iny
    iny
    iny
    iny

step_dst_x = $00
step_dst_y = $01
dist_x = $04
dist_y = $05
    ; If the unit has a movement vector, draw a dot halfway up
    lda unit_move_vector,x
    beq noplayer
    sty oam_used
    jsr get_unit_step_dest
    jsr unit_get_move_vector
    ldy oam_used
    lda step_dst_x
    sec
    adc dist_x
    asl a
    asl a
    asl a
    sta OAM+3,y
    adc unit_x,x
    ror a
    adc #4
    sta OAM+7,y
    lda step_dst_y
    clc
    adc dist_y
    asl a
    asl a
    asl a
    clc
    adc #FIELDTOP_Y - 1
    sta OAM+0,y
    adc unit_y,x
    ror a
    adc #FIELDTOP_Y / 2 - 1
    sta OAM+4,y
    lda #TILE_PIECE_CURSOR
    sta OAM+1,y
    lda #TILE_PATH_DOT
    sta OAM+5,y
    txa
    and #$01
    sta OAM+2,y
    sta OAM+6,y
    tya
    clc
    adc #8
    tay
  noplayer:

  ; Draw the cannon position
  txa
  and #$01
  tax
  lda player_cannon_x,x
  bmi nocannoncursor
    asl a
    asl a
    asl a
    adc #FIELDTOP_X+4
    sta OAM+3,y
    lda player_cannon_y,x
    asl a
    asl a
    asl a
    adc #FIELDTOP_Y-1+4
    sta OAM+0,y
    lda #TILE_A_BUTTON
    sta OAM+1,y
    txa
    sta OAM+2,y
    iny
    iny
    iny
    iny
  nocannoncursor:
  sty oam_used
  rts
.endproc

FURNI_MASK_MIN_TILE = $60
FURNI_MASK_NUM_TILES = 9
;;
; Draws a mask sprite in front of each sprite that's on a
; $60-$68 tile.
.proc draw_unit_furni_masks
pt_x = $00
pt_y = $01
tiletmp = $02

  ldx #2 * MAX_UNITS - 1
loop:
  lda unit_state,x
  bmi skip

  ; Decide whether the player is on a tile with a mask part
  lda unit_y,x
  lsr a
  adc #3
  lsr a
  lsr a
  sta pt_y
  lda unit_x,x
  lsr a
  adc #2
  lsr a
  lsr a
  sta pt_x
  jsr tile_to_addr
  bcs skip
  lda (fieldlo),y
  sta tiletmp
  sec
  sbc #FURNI_MASK_MIN_TILE
  cmp #FURNI_MASK_NUM_TILES
  bcs skip

  ; The tile should be drawn
  ldy oam_used
  lda pt_x
  asl a
  asl a
  asl a
  adc #FIELDTOP_X
  sta OAM+3,y
  lda pt_y
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y - 1
  sta OAM+0,y
  lda tiletmp
  sta OAM+1,y
  lda #%00100000
  sta OAM+2,y
  tya
  clc
  adc #4
  sta oam_used

skip:
  dex
  bpl loop
  rts
.endproc

.proc draw_unit_weapons
offsety = 0
tilenum = 1
attrs = 2
offsetx = 3
rawdirection = 4

  ldx #2 * MAX_UNITS - 1
unitloop:
 
  lda unit_state,x
  bmi skip_unit
  and #$0F
  cmp #UNIT_STATE_SWING_BAT+1
  bcc skip_unit
  bne not_swinging_bat
  lda unit_state,x
  lsr a
  lsr a
  lsr a
  lsr a
  tay
  and #$01
  ora #TILE_SWUNG_BAT
  sta tilenum
  lda direction_batoffsetx,y
  sta offsetx
  lda direction_batoffsety,y
  sta offsety
  lda direction_hflip,y
  sta attrs
  bpl have_vars

not_swinging_bat:
  cmp #UNIT_STATE_STUNNED
  bne skip_unit
  lda unit_hp,x
  ora #TILE_HPMARKER
  sta tilenum
  lda #0
  sta offsetx
  sta attrs
  lda #<-8
  sta offsety

have_vars:
  ldy oam_used
  lda unit_x,x
  clc
  adc offsetx
  clc
  adc #FIELDTOP_X
  sta OAM+3,y
  lda unit_y,x
  clc
  adc offsety
  clc
  adc #FIELDTOP_Y - 1 - 1
  sta OAM,y
  lda tilenum
  sta OAM+1,y
  txa
  and #$01
  ora attrs
  sta OAM+2,y
  iny
  iny
  iny
  iny
  sty oam_used
skip_unit:
  dex
  bpl unitloop
  rts
.endproc

.proc draw_units
rawdirection = $00
tilerow      = $01
attrs        = $02

  ldx #2 * MAX_UNITS - 1
  unitloop:
    lda unit_state,x
    bmi skip_unit
    lsr a
    lsr a
    lsr a
    lsr a
    tay
    lda direction_tilerow,y
    sta tilerow
    lda direction_hflip,y
    sta attrs
    txa
    and #$01
    tay  ; Y = player number
    lda player_sprite_origin,y
    adc tilerow
    sta tilerow

    ldy oam_used
    lda unit_x,x
    clc
    adc #FIELDTOP_X
    sta OAM+3,y
    lda unit_y,x
    clc
    adc #FIELDTOP_Y - 1 - 1
    sta OAM,y
    lda unit_state,x
    and #$0F
    ; XXX: This is where you'd change the translation of states to
    ; tile numbers.
    ora tilerow
    sta OAM+1,y
    txa
    and #$01
    ora attrs
    sta OAM+2,y
    iny
    iny
    iny
    iny
    sty oam_used
  skip_unit:
    dex
    bpl unitloop
  rts
.endproc

;;
; Draws bouncing B Button symbols for each player holding the
; B Button to request skipping to the next phase.
.proc draw_phase_skip_check
  lda #104
  ldy b_hold_time+0
  jsr do_one
  lda #144
  ldy b_hold_time+1
do_one:
  cpy #PHASE_SKIP_TIME/2
  bcc dont_draw
    ldx oam_used
    sta OAM+3,x
    asl a
    lda #0
    rol a
    sta OAM+2,x
    tya
    and #%00011000
    cmp #%00011000
    bcc :+
      lda #%00001000
    :
    lsr a
    lsr a
    lsr a
    adc #FIELDTOP_Y-1+8*FIELD_HT
    sta OAM+0,x
    lda #TILE_B_BUTTON
    sta OAM+1,x
    txa
    clc
    adc #4
    sta oam_used
  dont_draw:
  rts
.endproc

.proc draw_unit_carry_items
  ldx #0
  jsr oneplayer
  ldx #1
  oneplayer:
    ldy player_cur_unit,x
    lda unit_carry_item,y
    bmi not_carrying_anything
    lsr a
    jsr seek_to_furni_a
    lda held_furni_x,x
    sta $04
    lda #FIELDTOP_Y+192
    sta $05
    ldx unit_carry_item,y
    ldy #SHOPITEMS_SIZE
    lda (0),y  ; size bits
    sta $02
    iny
    lda (0),y  ; tilenumber
    sta $03
    jmp draw_furni_as_sprite
  not_carrying_anything:
  rts
.pushseg
.segment "RODATA"
held_furni_x:  .byte 16, 224
.popseg
.endproc
