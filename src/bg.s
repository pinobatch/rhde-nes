;
; RHDE background rendering
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
.p02

.import INITIAL_CASH

.segment "ZEROPAGE"
; Current row of fielddata
fieldlo: .res 1
fieldhi: .res 1
player_grass_color: .res 2

; PLAYFIELD INIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CODE"
; Playfield init

.proc init_playfield
  ; Clear inventory quantities to 0
  lda #0
  ldx #NUM_FURNIS * 2 - 1
clearinvloop:
  sta player_inv,x
  dex
  bpl clearinvloop

  ; Set up players'
  ldx #1
initplayerloop:
  lda #<INITIAL_CASH
  sta cash_lo,x
  lda #>INITIAL_CASH
  sta cash_hi,x
  lda #COLOR_GOODGRASS
  sta player_grass_color,x
  ; 2013-12-28: Give one missile silo for free
  lda #1
  sta player_inv+30,x
  dex
  bpl initplayerloop

  inx
  stx fullht_min_col
  ldy #30
  sty fullht_max_col
  txa
  clc
grassloop:
  ora #TILE_GRASS
  sta fielddata,x
  sta fielddata+256,x
  sta fielddata+512,x
  adc #1
  and #$03
  dey
  bne :+
  inx
  inx
  ldy #30
:
  inx
  bne grassloop

; Add borders
  ldx #FIELD_HT
  lda #<fielddata
  sta 0
  lda #>fielddata
  sta 1
borderloop:
  ldy #0
  lda #TILE_LBORDER
  sta (0),y
  ldy #29
  lda #TILE_RBORDER
  sta (0),y
  lda #32
  clc
  adc 0
  sta 0
  bcc :+
  inc 1
:
  dex
  bne borderloop

  lda #$4C
  sta bgup_jmp
  lda #<bgup_nop
  sta bgup_action
  lda #>bgup_nop
  sta bgup_action+1
  rts
.endproc

.proc make_road
dst = $00
dsthi = $01
road_map_type = $04
y1 = $05
y2 = $06
stamp_rows = $07

  sta road_map_type
  ldx #ROAD_HT - 1
init_xmax:
  lda #7
  sta xmax_1p,x
  lda #8
  sta xmin_2p,x
  dex
  bpl init_xmax
  lda road_map_type
  and #$02
  clc
  adc #2
  sta y1
  eor #$FF
  adc #ROAD_HT - 2 + 1
  sta y2
  lda road_map_type
  lsr a
  bcs adjust_xbounds_backslash

  ; Slash 
  ldx #0
:
  inc xmax_1p,x
  inc xmin_2p,x
  inx
  cpx y1
  bcc :-
  inc xmin_2p,x
  inc xmin_2p+1,x
  ldx y2
  dec xmax_1p,x
  inx
  dec xmax_1p,x
  inx
:
  dec xmax_1p,x
  dec xmin_2p,x
  inx
  cpx #ROAD_HT
  bcc :-
  bcs have_xbounds

adjust_xbounds_backslash:
  ldx #0
:
  dec xmax_1p,x
  dec xmin_2p,x
  inx
  cpx y1
  bcc :-
  dec xmax_1p,x
  dec xmax_1p+1,x
  ldx y2
  inc xmin_2p,x
  inx
  inc xmin_2p,x
  inx
:
  inc xmax_1p,x
  inc xmin_2p,x
  inx
  cpx #ROAD_HT
  bcc :-
have_xbounds:

  ; Now place the road tiles in the playfield
  ldx #0
  lda #<fielddata
  sta dst
  lda #>fielddata
  sta dsthi
roadpieceloop:
  ldy xmin_2p,x
  dey
  tya
  cmp xmax_1p,x
  bne not_single_road
  asl a
  tay
  lda #TILE_ROADS
  sta (dst),y
  iny
  lda #TILE_ROADS|$01
  sta (dst),y
  tya
  eor #$21
  tay
  lda #TILE_ROADS|$10
  sta (dst),y
  iny
  lda #TILE_ROADS|$11
  sta (dst),y
not_single_road:
  lda #64
  clc
  adc dst
  sta dst
  bcc :+
  inc dsthi
:
  inx
  cpx #ROAD_HT
  bcc roadpieceloop
  ldy y1
  jsr place_one_road_stamp
  ldy y2
  ; fall through
place_one_road_stamp:
  lda #4
  sta stamp_rows
  lda #0
  sta dst
  tya
  lsr a
  ror dst
  lsr a
  ror dst
  adc #>fielddata
  sta dsthi
  lda xmax_1p,y
  asl a
  ora dst
  sta dst
  lda road_map_type  ; bit 0 false: slash; true: backslash
  lsr a
  ldx #0
  bcc :+
  ldx #16
:
@rowloop:
  ldy #0
@tileloop:
  lda road_stamps,x
  sta (dst),y
  inx
  iny
  cpy #4
  bcc @tileloop
  lda dst
  clc
  adc #32
  sta dst
  bcc :+
  inc dst+1
:
  dec stamp_rows
  bne @rowloop
  
  rts
.pushseg
.segment "RODATA"
road_stamps:
  mbyt "002D2E2F"  ; slash
  mbyt "003D3E3F"
  mbyt "2D2E2F00"
  mbyt "3D3E3F00"
  mbyt "2A2B2C00"  ; backslash
  mbyt "3A3B3C00"
  mbyt "002A2B2C"
  mbyt "003A3B3C"
.popseg
.endproc

.proc place_initial_houses
  lda xmax_1p+11
  asl a
  adc #<-6
  tay
  ldx #3
  jsr place_one_house
  lda xmin_2p+0
  asl a
  adc #<-8
  tay
  ldx #19
  jsr place_one_house
  rts

place_one_house:
  jsr seek_fielddata_row_y
  txa
  ora fieldlo
  sta fieldlo
  ldy #0
  lda #TILE_OV_STRAY_BLOCK
@toploop:
  sta (fieldlo),y
  iny
  cpy #8
  bcc @toploop
  ldy #224
@bottomloop:
  sta (fieldlo),y
  iny
  cpy #232
  bcc @bottomloop
  ldy #32
  clc
  ldx #0
@sidesloop:
  lda house_side_walls,x
  sta (fieldlo),y
  tya
  clc
  adc #7
  tay
  inx
  lda house_side_walls,x
  sta (fieldlo),y
  tya
  adc #32-7
  tay
  cpy #224
  bcc @sidesloop

  ; Place one bed
  ldy #161
  lda fieldlo
  and #$1F
  cmp #$0E
  bcc :+
  ldy #165
:
  lda #TILE_BED
  sta (fieldlo),y
  iny
  lda #TILE_BED+1
  sta (fieldlo),y
  tya
  clc
  adc #31
  tay
  lda #TILE_BED+16
  sta (fieldlo),y
  iny
  lda #TILE_BED+17
  sta (fieldlo),y
  rts
.pushseg
.segment "RODATA"
house_side_walls:
  .byte TILE_OV_STRAY_BLOCK, TILE_OV_STRAY_BLOCK
  .byte TILE_OV_STRAY_BLOCK, TILE_OV_DOOR_NS, TILE_OV_STRAY_BLOCK
  .byte TILE_OV_STRAY_BLOCK, TILE_OV_STRAY_BLOCK
.popseg
.endproc

FIELD_DST_TOP = $2040
FIELD_BORDER_TOP = ::FIELD_DST_TOP - $20
SHOP_FRAME_TOP = FIELD_DST_TOP + (FIELD_HT - SHOP_FRAME_HT) * 32
FIELD_BORDER_BOTTOM = ::FIELD_DST_TOP + ::FIELD_HT * 32

.proc make_road_border
attrblk = $07
attrbuf = $08
  ; Make the attributes for the road tiles
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$23
  sta PPUADDR
  lda #$C0
  sta PPUADDR
  ldy #0
  jsr get_one_attribute_row
attrloop:
  jsr get_one_attribute_row
  jsr copy_attribute_row
  iny
  jsr get_one_attribute_row
  iny
  cpy #ROAD_HT
  bcc attrloop
  dey
  jsr get_one_attribute_row
  jsr copy_attribute_row
  jsr copy_attribute_row

  ; Now draw top and bottom borders of playfield including this road
  lda #>FIELD_BORDER_TOP
  sta PPUADDR
  .if <::FIELD_BORDER_TOP <> >::FIELD_BORDER_TOP
    lda #<::FIELD_BORDER_TOP
  .endif
  sta PPUADDR
  lda #TILE_TLBORDER
  sta PPUDATA
  ldx xmax_1p+0
  dex
  lda #TILE_TBORDER
:
  sta PPUDATA
  sta PPUDATA
  dex
  bne :-
  lda #TILE_TLINLET
  sta PPUDATA
  lda #TILE_ROADS|$10
  sta PPUDATA
  lda #TILE_ROADS|$11
  sta PPUDATA
  lda #TILE_TRINLET
  sta PPUDATA
  ldx xmin_2p+0
  lda #TILE_TBORDER
:
  sta PPUDATA
  sta PPUDATA
  inx
  cpx #14
  bcc :-
  lda #TILE_TRBORDER
  sta PPUDATA

  lda #>FIELD_BORDER_BOTTOM
  sta PPUADDR
  lda #<FIELD_BORDER_BOTTOM
  sta PPUADDR
  lda #TILE_BLBORDER
  sta PPUDATA
  ldx xmax_1p+11
  dex
  lda #TILE_BBORDER
:
  sta PPUDATA
  sta PPUDATA
  dex
  bne :-
  lda #TILE_BLINLET
  sta PPUDATA
  lda #TILE_ROADS|$00
  sta PPUDATA
  lda #TILE_ROADS|$01
  sta PPUDATA
  lda #TILE_BRINLET
  sta PPUDATA
  ldx xmin_2p+11
  lda #TILE_BBORDER
:
  sta PPUDATA
  sta PPUDATA
  inx
  cpx #14
  bcc :-
  lda #TILE_BRBORDER
  sta PPUDATA
  rts

get_one_attribute_row:
  ldx #0
  stx attrblk
@loop:
  lsr attrbuf,x
  lsr attrbuf,x
  lda attrblk
  cmp xmin_2p,y
  bcc @notpast2
  lda #$40
  bne @have_orvalue
@notpast2:
  cmp xmax_1p,y
  bcc @notpast1
  lda #$80
@have_orvalue:
  ora attrbuf,x
  sta attrbuf,x
@notpast1:
  inc attrblk
  lda attrblk
  lsr a
  tax
  cpx #8
  bcc @loop
  rts

copy_attribute_row:
  ldx #0
:
  lda attrbuf,x
  sta PPUDATA
  inx
  cpx #8
  bcc :-
  rts
.endproc

;;
; Loads fieldlo and fieldhi with the start address of row Y
; and clears C.
.proc seek_fielddata_row_y
  tya
.endproc
.proc seek_fielddata_row_a
  lsr a
  ror a
  ror a
  sta fieldhi
  and #%11000000
  ror a
  sta fieldlo
  lda fieldhi
  and #%00000011
  adc #>fielddata
  sta fieldhi
  rts
.endproc

;;
; Determines whether tile coordinates are within the playfield and
; gets the address of a tile point if so.
; @param 0,1 X,Y coordinates in tiles
; @return Address of tile row in fieldlo-fieldhi, Y = x coord;
; carry set if out of bounds
.proc tile_to_addr
pt_x = 0
pt_y = 1
  ldy pt_x
  beq is_offscreen
  cpy #29
  bcs is_offscreen
  lda pt_y
  cmp #FIELD_HT
  bcs is_offscreen
  jmp seek_fielddata_row_a
is_offscreen:
  sec
  rts
.endproc

; BACKGROUND UPDATE SCHEDULER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Updates the 3 columns around the cursor.  If an update is already
; in progress, it instead schedules an update for the whole side.
; Assumes the cursor is on the same side as the player.
; 
; Updating 3 columns takes one frame; updating a whole side takes 3.
; @param X player (0 or 1)
.proc update_around_cursor
  ldy cursor_x,x
  ; fall through
.endproc

;; Updates the three columns around column Y (1=left, 28=right).
; @param Y column to update
; @param X player (0 or 1) to update if fallback
.proc update_around_column_y
  lda copy_region_width
  beq no_copy_in_progress
  lda #DIRTY_SIDE
  ora side_dirty,x
  sta side_dirty,x
  rts
no_copy_in_progress:
  dey
  sty copy_region_left
  lda #3
  sta copy_region_width
  rts
.endproc

;;
; Begins or continues a playfield update job.
.proc build_choose_redraw
  lda copy_region_width
  bne continue_current_job
  lda side_dirty
  and #DIRTY_SIDE
  beq not_redraw_left
  and side_dirty+1
  beq redraw_only_left
  eor side_dirty
  sta side_dirty
  lda side_dirty+1
  and #<~DIRTY_SIDE
  sta side_dirty+1
  lda #0
  sta copy_region_left
  lda #30
  bne have_region_width
redraw_only_left:
  lda side_dirty
  and #<~DIRTY_SIDE
  sta side_dirty
  lda #0
  beq redraw_aplus16
not_redraw_left:
  lda side_dirty+1
  and #DIRTY_SIDE
  beq no_redraw_just_yet
  eor side_dirty+1
  sta side_dirty+1
  lda #14
redraw_aplus16:
  sta copy_region_left
  lda #16
have_region_width:
  sta copy_region_width
continue_current_job:
  lda #<bgup_field
  sta bgup_action
no_redraw_just_yet:
  rts
.endproc

SHOPDRAW_DONE = 0
SHOPDRAW_FRAME1 = 2
SHOPDRAW_FRAME2 = 4
SHOPDRAW_FRAME3 = 6
SHOPDRAW_FRAME4 = 8
SHOPDRAW_LINE34 = 10
SHOPDRAW_LINECASH = 12
STOCKDRAW_LINE34 = 14
STOCKDRAW_LINE56 = 16
STOCKDRAW_LINE78 = 18
STOCKDRAW_COUNTS = 20

;;
; More complicated update scheduler for furnish phase.
.proc shop_choose_redraw
  ; If a map update is in progress, let it finish.  Or if one is
  ; required, start it.  Map lag is more noticeable than form lag.
  jsr build_choose_redraw
  lda copy_region_width
  beq no_map_update
  rts
no_map_update:

  ; If the view type has changed, begin a view change.
  ; This interrupts updating the view contents.
  ldx enclose_turn
  lda form_desired_view,x
  cmp form_cur_view,x
  beq no_view_change

  ; Unless switching to no view, a blank view (just the frame) is
  ; acceptable as a temporary substitute because it signals a
  ; transition in progress.
  cmp #FRAME_NONE
  bne not_changing_to_none
  sta form_cur_view,x
  lda #DIRTY_SIDE
  ora side_dirty,x
  sta side_dirty,x
force_shophide:
  lda #<bgup_shophide
  sta bgup_action
  rts

not_changing_to_none:
  lda form_cur_view,x
  cmp #FRAME_BLANK
  beq no_view_change
need_view_change:
  lda #FRAME_BLANK
  sta form_cur_view,x
  lda #<bgup_shopframe
  sta bgup_action
  lda #SHOPDRAW_FRAME1
  sta enclose_state
  rts
no_view_change:

  ; If an update is still in progress, continue it
  ldx enclose_state
  beq update_not_in_progress
  lda shop_updaters-1,x
  pha
  lda shop_updaters-2,x
  pha
  rts

update_not_in_progress:
  ; If an update is requested for this player, begin the update.
  ldx enclose_turn
  lda #DIRTY_FORM|DIRTY_FORMEND
  and side_dirty,x
  bne begin_player_x_update
  
  ; If an update is requested for other player, begin the update.
  txa
  eor #$01
  tax
  lda #DIRTY_FORM|DIRTY_FORMEND
  and side_dirty,x
  bne begin_player_x_update
  
  ; If no update is in progress or requested, make absolutely
  ; sure the correct view is showing.
  ldx enclose_turn
  lda form_desired_view,x
  cmp form_cur_view,x
  beq no_fill_frame

fill_frame:
  ; Request full update of form contents
  ldx enclose_turn
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x

  ; And load the nametable
  lda #SHOPDRAW_DONE
  sta enclose_state
  lda #<bgup_shopnt
  sta bgup_action
  lda form_desired_view,x
  sta form_cur_view,x
  lsr a
  lda #>stock_nt
  ldy #<stock_nt
  bcs nt_is_stock
  lda #>shop_nt
  ldy #<shop_nt
nt_is_stock:
  jmp shop_load_nt

no_fill_frame:
  lda enclose_turn
  eor #$01
  and #$01
  sta enclose_turn
  rts

begin_player_x_update:
  stx enclose_turn
  and #DIRTY_FORM
  beq only_formend

  ; acknowledge
  eor side_dirty,x
  sta side_dirty,x
  lda form_cur_view,x
  lsr a
  bcs start_stock_update
  lda #SHOPDRAW_LINE34-2
  sta enclose_state
  jmp shop_12

start_stock_update:
  lda #STOCKDRAW_LINE34-2
  sta enclose_state
  jmp stock_12

only_formend:
  ; acknowledge
  lda #<~DIRTY_FORMEND
  and side_dirty,x
  sta side_dirty,x
  jmp shopfurni

.pushseg
.segment "RODATA"
shop_updaters:
  .addr erase_80-1, erase_90-1, erase_A0-1, fill_frame-1
  .addr shop_34-1, shopcash-1
  .addr stock_34-1, stock_56-1, stock_78-1, stock_counts-1
.popseg

erase_80:
  lda #$08
  bne shop_choose_erase_A
erase_90:
  lda #$09
  bne shop_choose_erase_A
erase_A0:
  lda #$0A
shop_choose_erase_A:
  ldx enclose_turn
  beq not_player_2
  ora #$04
not_player_2:
  sta bgup_arg
  lda #<bgup_clear2
  sta bgup_action
  inc enclose_state
  inc enclose_state
  rts

shop_12:
  ldy #0
  beq shop_2lines
shop_34:
  ldy #2
shop_2lines:
  inc enclose_state
  inc enclose_state
  tya
  lsr a
  ora #$08
  ldx enclose_turn
  beq :+
  ora #$04
:
  sta bgup_arg
  lda #<bgup_vwf
  sta bgup_action
  tya
  jmp draw_two_shopitems

stock_12:
  ldy #0
  beq stock_2lines
stock_34:
  ldy #2
  bne stock_2lines
stock_56:
  ldy #4
  bne stock_2lines
stock_78:
  ldy #6
stock_2lines:
  inc enclose_state
  inc enclose_state
  tya
  lsr a
  lsr a
  bcc :+
  ora #$80
:
  ora #$08
  ldx enclose_turn
  beq :+
  ora #$04
:
  sta bgup_arg
  lda #<bgup_8tiles
  sta bgup_action
  tya
  jmp draw_two_stockitems

stock_counts:
  lda #0
  sta enclose_state
  lda #<bgup_vwf
  sta bgup_action
  ; Draw counts to tiles A0-AF ($0A00) for 1P or E0-EF ($0E00) for 2P
  lda #$0A
  ldx enclose_turn
  beq :+
  ora #$04
:
  sta bgup_arg
  ; 2014-03-01: Stock has no form-end handler; it redraws entirely
  ; (when changing pages) or not at all (when moving the cursor).
  ; Make sure shop's form-end (which draws the picture of the
  ; selected item at the lower left corner) isn't drawn.
  lda #<~DIRTY_FORMEND
  and side_dirty,x
  sta side_dirty,x
  jmp stock_draw_counts

shopcash:
  lda #0
  sta enclose_state
  lda #$0A  ; $0A00 or $0E00
  ldx enclose_turn
  beq :+
  ora #$04
:
  sta bgup_arg
  lda #<bgup_vwf
  sta bgup_action
  lda #DIRTY_FORMEND
  ora side_dirty,x
  sta side_dirty,x
  jmp shop_draw_cash

shopfurni:
  jsr clear_8_tiles
  lda #$0A  ; $0A00 or $0E00
  ldx enclose_turn
  beq :+
  ora #$04
:
  sta bgup_arg
  lda #<bgup_8tiles
  sta bgup_action
  ldx enclose_turn
  lda form_pageitem,x
  cmp form_pagelen,x
  bcc :+
  rts
:
  asl a
  ora enclose_turn
  tax
  lda form_pageitems,x
  pha

  ; Draw how many the player owns
  asl a
  ora enclose_turn
  tax
  lda player_inv,x
  beq not_owned
  jsr bcd8bit
  pha
  lda $00  ; high digit
  beq noHighDigits
  ora #$30
  ldx #75
  jsr vwfPutTile
noHighDigits:
  pla
  ora #$30
  ldx #88
  jsr vwfPutTile
not_owned:

  pla
  ldx #0  ; 0: tiles B0-B3, 64: tiles B4-B7, only using B0-B3
  jmp load_menu_furni
.endproc

.proc bottom_row_choose_redraw
  lda #0
  sta form_cur_view+0
  sta form_cur_view+1
  jmp shop_choose_redraw::force_shophide
.endproc

; BACKGROUND UPDATE EXECUTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "ZEROPAGE"
bgup_jmp: .res 1
bgup_action: .res 2
bgup_arg: .res 1

; Playfield region to update over the next frames
copy_region_left: .res 1
copy_region_width: .res 1

; Columns that are safe for full height updates
; Use this when drawing furnish phase UI
fullht_min_col: .res 1
fullht_max_col: .res 1

.segment "CODE"

.align 32
bgup_nop:           rts
bgup_shopframe:     jmp shop_draw_frame
bgup_clear2:        jmp clear_vwf_rows_bgup_arg
bgup_shopnt:        jmp shop_copy_nt
bgup_shophide:      jmp shop_erase_frame_bottom
bgup_field:         jmp copy_field_cols
bgup_vwf:           jmp copyLineImg_bgup_arg
bgup_8tiles:        jmp copy_8_tiles_bgup_arg
;;
; Waits for vertical blanking, copies the display list to OAM,
; copies data to video memory, and resets the scroll position.
.proc bgup_vsync
  lda nmis
:
  cmp nmis
  beq :-
  lda #0
  sta OAMADDR
  sta PPUMASK
  lda #>OAM
  sta OAM_DMA
  jsr bgup_jmp
  lda #<bgup_nop
  sta bgup_action
  ; fall through
.endproc
.proc screen_back_on
  ; Turn the screen on
  ldx #248
  ldy #236
  lda #VBLANK_NMI|BG_0000|OBJ_1000|3
  sec
  jmp ppu_screen_on
.endproc

.proc copy_field_cols
maxwidth = 0
  ldx copy_region_left
  lda #VBLANK_NMI|VRAM_DOWN
  sta PPUCTRL

  ; Find out how much we can copy per frame
  ; Copy 15 columns on PAL NES and 6 on NTSC NES
  ; Use NTSC NES value for Dendy because extending vblank on Dendy
  ; requires ending rendering early, and we don't do that yet
  ldy #6
  lda tvSystem
  lsr a
  bcc :+
  ldy #15
:
  cpy copy_region_width
  bcc copy_loop
  ldy copy_region_width
  bne copy_loop
  jmp copy_done

copy_loop:
  lda #>FIELD_DST_TOP
  sta PPUADDR
  txa
  ora #<FIELD_DST_TOP
  sta PPUADDR
  .repeat ::FIELD_HT - ::SHOP_FRAME_HT, I
    lda fielddata+32*I,x
    sta PPUDATA
  .endrepeat
  cpx fullht_min_col
  bcc skip_bottom
  cpx fullht_max_col
  bcs skip_bottom
  .repeat ::SHOP_FRAME_HT, I
    lda fielddata+32*(::FIELD_HT - ::SHOP_FRAME_HT + I),x
    sta PPUDATA
  .endrepeat
skip_bottom:
  inx
  dey
  beq copy_done
  jmp copy_loop
copy_done:

  ; new width = old width + old x - new x
  txa
  eor #$FF
  sec
  adc copy_region_left
  clc
  adc copy_region_width
  sta copy_region_width
  stx copy_region_left
  rts
.endproc

.segment "RODATA"
shop_frame_left:
  .byte 0, 18
.segment "CODE"

.proc clear_vwf_rows_bgup_arg
  ldx bgup_arg
  ldy #16
.endproc
;;
; Fills 512 bytes (32 tiles) of pattern table starting at
; PPU $XX00 (tile $XX0) with blank tiles of color 1.
; Usually X=$08, $0A, $0C, or $0E.
.proc clear_vwf_rows
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  stx PPUADDR
  sta PPUADDR
  ldx #$FF
rowloop:
  .repeat 8
    stx PPUDATA
  .endrepeat
  .repeat 8
    sta PPUDATA
  .endrepeat
  dey
  bne rowloop
  rts
.endproc

.proc copyLineImg_bgup_arg
  lda bgup_arg
  ldy #0
  jmp copyLineImg
.endproc

;;
; @param enclose_turn player (0 or 1)
.proc shop_draw_frame
dstlo = $00
dsthi = $01
  ldx enclose_turn

  ; Write the top border
  lda #>SHOP_FRAME_TOP
  sta dsthi
  sta PPUADDR
  lda shop_frame_left,x
  ora #<SHOP_FRAME_TOP
  sta dstlo
  sta PPUADDR
  bit PPUDATA
  lda #TILE_BLACK
  jsr wr10

  ; Write the body
  ldx #SHOP_FRAME_HT - 1
toploop:
  lda dsthi
  sta PPUADDR
  lda dstlo
  sta PPUADDR
  lda #TILE_FRAME_LEFT
  sta PPUDATA
  lda #TILE_WHITE
  jsr wr10
  lda #TILE_FRAME_RIGHT
  sta PPUDATA
  dex
  bne toploop

  ; Don't overwrite the columns
  ldx enclose_turn
  bne is_player2
  lda #12
  sta fullht_min_col
  bne setframe_done
is_player2:
  lda #18
  sta fullht_max_col
setframe_done:

  ; Write the bottom border
  lda dsthi
  sta PPUADDR
  lda dstlo
  sta PPUADDR
  bit PPUDATA
  lda #TILE_FRAME_BOTTOM
wr10:
  ldy #(::SHOP_FRAME_WID - 2) / 2
:
  .repeat 2
    sta PPUDATA
  .endrepeat
  dey
  bne :-
  clc
  lda dstlo
  adc #32
  sta dstlo
  bcc :+
  inc dsthi
:
  rts
.endproc

.proc shop_erase_frame_bottom
  lda form_cur_view+0
  bne left_view_showing
  ;lda #0
  sta fullht_min_col
  lda #<FIELD_BORDER_BOTTOM + 1
  jsr oneside
left_view_showing:  
  lda form_cur_view+1
  bne right_view_showing
  lda #30
  sta fullht_max_col
  lda #<FIELD_BORDER_BOTTOM + 19
oneside:
  ldy #>FIELD_BORDER_BOTTOM
  sty PPUADDR
  sta PPUADDR
  lda #TILE_BBORDER
  ldy #10
:
  sta PPUDATA
  dey
  bne :-
right_view_showing:
  rts
.endproc

;;
; Clears 8 tiles in transfer buffer to color 1.
.proc clear_8_tiles
  lda #$FF
  ldy #0
planeloop:
  ldx #8
byteloop:
  sta lineImgBuf,y
  iny
  dex
  bne byteloop
  eor #$FF
  cpy #$80
  bcc planeloop
  rts
.endproc

.proc copy_8_tiles_bgup_arg
  ldy #0
  lda bgup_arg
  bpl copy_8_tiles
  ldy #$80
.endproc
.proc copy_8_tiles
  sta PPUADDR
  sty PPUADDR
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a  ; A = 0
loop:
  tax
  .repeat 8, I
    ldy lineImgBuf+I,x
    sty PPUDATA
  .endrepeat
  clc
  adc #8
  bpl loop
  rts
.endproc

;;
; Copies a 70-byte nametable segment to the top border and interior
; of a shop frame.
; @param enclose_turn 0: player 1; 1: player 2
.proc shop_copy_nt
dstlo = 0
dsthi = 1
  ldx enclose_turn
  lda #>SHOP_FRAME_TOP
  sta dsthi
  lda #<SHOP_FRAME_TOP
  sec  ; Skip the left border tile, which is never included
  adc shop_frame_left,x
  sta dstlo
  ldy #0
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
  ldx #(SHOP_FRAME_WID - 2)/2
byteloop:
  .repeat 2
    lda lineImgBuf,y
    sta PPUDATA
    iny
  .endrepeat
  dex
  bne byteloop
  cpy #(SHOP_FRAME_WID - 2)*SHOP_FRAME_HT
  bcc rowloop
  rts
.endproc

