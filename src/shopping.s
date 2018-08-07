;
; RHDE furnish phase: main loop and shop window
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

FURNISH_PHASE_DURATION = 30

.export shopitems

; floodscratch, a 4*FIELD_HT = 96 byte scratchpad area, is defined in
; the flood fill used by the build phase.  But because the furnish
; phase doesn't need flood fill, it gets reused to store the logical
; layout of the current stock/shop form.
; Offsets 0, 2, 4, 6, 8, 10, 12, 14 for each player store an item
; number or >=$80 for no item.

;form_whateverelse = form_pageitems + STOCK_PAGELEN * 2

.segment "CODE"

.proc furnish_phase
  lda #0
  sta enclose_state
  sta time_tenths
  sta time_subtenths
  lda #FURNISH_PHASE_DURATION
  sta phase_seconds
  lda #8
  sta cursor_x
  lda #21
  sta cursor_x+1
  ldx #1
initloop:
  stx cur_turn
  stx enclose_turn
  ldy #11
  sty cursor_y,x
  ldy #0
  sty form_cur_view,x
  jsr setup_stock_view
  jsr build_choose_redraw
  jsr bgup_vsync
  jsr update_sound
  ldx cur_turn
  dex
  bpl initloop
  jsr shop_load_heading_tiles

loop:
  jsr read_pads
  ldx #1
:
  stx cur_turn
  jsr furnish_move_cursor
  ldx cur_turn
  dex
  bpl :-

  jsr shop_choose_redraw
  ldx #0
  stx oam_used
  jsr shop_frame_corners
  jsr build_draw_timer
  ldx #1
:
  stx cur_turn
  jsr furnish_draw_cursor
  ldx cur_turn
  dex
  bpl :-
  jsr draw_debughex
  ldx oam_used
  jsr ppu_clear_oam
  jsr bgup_vsync
  jsr update_sound

  jsr countdown_logic
  bcs not_54321
  lda #SFX_COUNTDOWN
  jsr start_sound
not_54321:
  lda phase_seconds
.if 1
  bne loop
.else
  lda new_keys+0
  and #KEY_START
  beq loop
.endif

  ; Prepare for build phase
  jsr bottom_row_choose_redraw
  ldx #0
  stx enclose_state
  jsr ppu_clear_oam
  lda #DIRTY_SIDE
  sta side_dirty+0
  sta side_dirty+1
  jsr bgup_vsync
  jmp update_sound
.endproc

.proc furnish_move_cursor
  lda form_cur_view,x
  cmp form_desired_view,x
  beq is_desired_view
  rts
is_desired_view:
  cmp #FRAME_STOCK
  bne not_shop_view
  jmp stock_move_cursor
not_shop_view:
  cmp #FRAME_SHOP
  beq shop_move_cursor
  jmp placement_move_cursor
.endproc

.proc shop_move_cursor
  lda #<~0
  ldy form_buy_timeout,x
  beq :+
  dec form_buy_timeout,x
  lda #<~KEY_A
:
  and new_keys,x
  bpl notA
  jmp buy_item
notA:
  cmp #KEY_B
  bcs isLeft
  lsr a
  lsr a
  bcc notLeft
  
  ; Left: Go to stock view
isLeft:
  lda #SFX_TURN_PAGE
  jsr start_sound
  jmp setup_stock_view
notLeft:
  
  lsr a
  bcc notDown
  
  ; Down: move to next item (or first item on next page)
  lda #DIRTY_FORMEND
  ora side_dirty,x
  sta side_dirty,x
  inc form_pageitem,x
  lda form_pageitem,x
  cmp form_pagelen,x
  bcc notUp
  lda #SFX_TURN_PAGE
  jsr start_sound
  ldx cur_turn
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x
  inc form_pagenum,x
  lda form_pagenum,x
  asl a
  asl a
  tay
  jsr find_affordable_shopitems
  cpx #2
  bcs down_nowrap
  lda #0
  sta form_pagenum,x
  tay
  jsr find_affordable_shopitems
down_nowrap:
  ldx cur_turn
down_finished:
  lda #0
  sta form_pageitem,x
  rts
notDown:
  lsr a
  bcc notUp

  ; Up: move to previous item (or last item on previous page)
  lda #DIRTY_FORMEND
  ora side_dirty,x
  sta side_dirty,x
  dec form_pageitem,x
  bmi isUp
notUp:
  rts
isUp:
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x
.endproc
.proc find_previous_shopitem
  lda #SFX_TURN_PAGE
  jsr start_sound
  ldx cur_turn
  dec form_pagenum,x
  bpl up_nowrap
  lda #(NUM_FURNIS - 1)/4
  sta form_pagenum,x
up_nowrap:
  lda form_pagenum,x
  asl a
  asl a
  tay
  jsr find_affordable_shopitems
  ldx cur_turn
  ldy form_pagelen,x
  beq up_wrap_not_finished
  dey
  bpl have_pageitem
up_wrap_not_finished:
  dec form_pagenum,x
  bpl up_nowrap
  ldy #0
  sty form_pagelen,x
  sty form_pagenum,x
have_pageitem:
  sty form_pageitem,x
  rts
.endproc

;;
; Takes one of the item under the player's cursor and transfers it
; to the player's stock.
; @param X player
.proc buy_item
itemdatalo = $00
itemdatahi = $01
itemoffset = $02
price = $03

  ; Don't allow buying items that don't exist
  lda form_pageitem,x
  cmp form_pagelen,x
  bcs range_error
  cpx #1
  jsr seek_to_pageitem_a
  tya
  cpx #1
  rol a
  sta itemoffset

  ; Don't allow owning more than 99 of something
  tay
  lda player_inv,y
  cmp #99
  bcc validbuy1
range_error:
  rts
validbuy1:

  ; Ensure the player has enough money
  ldy #SHOPITEMS_PRICE  ; price
  lda (itemdatalo),y
  sta price
  lda cash_hi,x
  bne affordable
  lda cash_lo,x
  cmp price
  bcc unaffordable
affordable:
  ; Deduct the price
  lda cash_lo,x
  sec
  sbc price
  sta cash_lo,x
  lda cash_hi,x
  sbc #0
  sta cash_hi,x

  ; Set the particle
  lda #15
  sta form_buy_timeout,x
  ldy #SHOPITEMS_TILE
  lda (itemdatalo),y
  ; convert fence/flower
  cmp #$50
  bcs :+
  cmp #$30
  lda #$50 >> 1
  rol a
:
  sta form_shopparticletn,x
  dey
  lda (itemdatalo),y
  sta form_shopparticlesz,x


  ; Add 1 to the number of this item owned
  txa
  ldx itemoffset
  inc player_inv,x
  pha
  lda #SFX_SCANBEEP
  jsr start_sound
  pla
  tax

  ; Recalculate what player can afford
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x
  lda form_pagenum,x
  asl a
  asl a
  tay
  jsr find_affordable_shopitems
  ldx cur_turn
  lda form_pagelen,x
  bne :+
  jmp find_previous_shopitem
:
  lda form_pageitem,x
  cmp form_pagelen,x
  bcc cursor_ok
  sbc #1
  sta form_pageitem,x
cursor_ok:
unaffordable:
  rts
.endproc

.proc setup_shop_view
  ldx cur_turn
  lda #FRAME_SHOP
  sta form_desired_view,x
  lda #0
  sta form_buy_timeout,x
  sta form_pagenum,x
  sta form_pageitem,x
  tay
  ; fall through to find_affordable_shopitems
.endproc
;;
; Finds the first four shop items that are affordable for player X,
; skipping the first Y.  Pass Y=$FF and invert skipitems to see how
; many affordable items there are.
; @param X player number (0 or 1)
; @param Y number of items to skip (usually 0, 4, 8, 12, or 255)
; @return X = number of items found times 2, plus player number
;         Y = remaining skips
;         form_pageitems: beginning filled with affordable items
;         form_pagelen: number of items found (0 to 4)
.proc find_affordable_shopitems
itemlo = $00
itemhi = $01
my_cash_lo = $02
my_cash_hi = $03
itemid = $04
skipitems = $05
shopitems_price = shopitems + 7
  sty skipitems
  lda cash_lo,x
  sta my_cash_lo
  lda cash_hi,x
  sta my_cash_hi
  ldy #0
  sty itemid
  sty itemlo
  ldy #<shopitems_price
  lda #>shopitems_price
  sta itemhi
  ; X = starting offset into array of form_pageitems
loop:
  ; All items are priced under 250 cio or less.  Therefore a player
  ; with at least 250 cio can afford everything.
  lda my_cash_hi
  bne affordable
  lda my_cash_lo
  cmp (itemlo),y
  bcc next_item
affordable:
  lda skipitems
  beq add_item
  dec skipitems
  jmp next_item
add_item:
  lda itemid
  sta form_pageitems,x
  inx
  inx
  cpx #2 * SHOP_PAGELEN
  bcs done
next_item:
  tya
  clc
  adc #16
  tay
  bcc :+
  inc itemhi
:
  inc itemid
  lda itemid
  cmp #NUM_FURNIS
  bcc loop
done:
  ldy cur_turn
  txa
  lsr a
  sta form_pagelen,y
  beq zero_length_page

  ; Move cursor above pageitem
  clc
  sbc form_pageitem,y  ; A = length - 1 - line number
  bcs ok_pagelen
  lda form_pagelen,y
  sbc #0
  sta form_pageitem,y
ok_pagelen:
  lda form_pagelen,y
zero_length_page:
  ldy skipitems
  rts
.endproc

.export fill_pageitems_done
fill_pageitems_done = find_affordable_shopitems::done

.segment "RODATA"
.align 16

; Physical format:
; $00-$03 unassigned
; $04     Next item in rotation cycle
; $05     bit 7: tall; bit 6: wide; bit 5: indoor
; $06     base tile number
; $07     price in cio (1-250)
; $08-$0F name in nul-term ASCII
.macro shopitem name, price, width, height, indoor, tileno, rotnext
.local itemnum
itemnum = (* - shopitems) / 16
  .assert itemnum < 64, error, "more than 64 items cause Bad Things"
  .ident(.concat("item_", name)) = <itemnum
  .byte $00,$00,$00,$00
  .ifblank rotnext
    .byte (* - shopitems) / 16
  .else
    .byte .ident(.concat("item_", rotnext))
  .endif
  .byte (((height) - 1) << 7) | (((width) - 1) << 6) | ((indoor) << 5)
  .byte tileno, price
  .byte name
  .res 8 - .strlen(name), $00
.endmacro
; Rotated furnis are beyond NUM_FURNIS.  They cannot be purchased,
; and trying to purchase one follows a rotnext chain until the number
; is less than NUM_FURNIS
;          name       prc  w  h ind tile rotnext
shopitems:
  shopitem "bed",      85, 2, 2, 1, $60
  shopitem "table",    72, 1, 1, 1, $5F
  shopitem "chair",    35, 1, 2, 1, $63
  shopitem "rug",      65, 1, 2, 1, $69, "rug_rot"
  shopitem "sofa",     35, 2, 1, 1, $5A
  shopitem "bookcase", 90, 1, 2, 1, $62
  shopitem "fridge",   48, 1, 2, 1, $65
  shopitem "oven",     72, 1, 2, 1, $66
  shopitem "trashcan", 20, 1, 1, 1, $5e
  shopitem "sink",     80, 1, 2, 1, $67
  shopitem "toilet",   72, 1, 2, 1, $68
  shopitem "bathtub",  50, 2, 1, 1, $58
  shopitem "ficus",    50, 1, 2, 1, $64
  shopitem "fence",     8, 1, 1, 0, $38
  shopitem "flowers",   3, 1, 1, 0, $28
  shopitem "silo",    120, 2, 2, 0, $6D
  shopitem "bat",      20, 1, 1, 1, $57
shopitems_end:
  shopitem "flowers2", 65, 1, 1, 0, $29, "flowers"
  shopitem "rug_rot",  65, 2, 1, 1, $5C, "rug"
  shopitem "siloused", 65, 1, 2, 0, $6F, "silo"
allitems_end:

;.out .sprintf("rug is #%03d", item_rug)

.assert (shopitems_end - shopitems) / 16 = NUM_FURNIS, error, .sprintf("NUM_FURNIS is incorrect; change it to %d in global.inc", (shopitems_end - shopitems) / 16)
.assert (allitems_end - shopitems) < 16*32, error, "WAY too many furnis"
NUM_ALL_FURNIS = <((allitems_end - shopitems) / 16)

.segment "CODE"
;;
; Counts the tiles that earn rent on one side of the field.
; Tiles $01-$02 (reachable floor) and $58-$7F (furniture)
; count for rent. Others don't.
; Should take less than half a draw period, allowing calculating
; area for both players in one frame.
; @param X player number
.proc count_rent_tiles
count_lo = $00
count_hi = $01
player = $04
rownum = $05
xmin = $06

  stx player
  lda #>fielddata
  sta fieldhi
  ldx #0
  stx fieldlo
  stx count_lo
  stx count_hi
rowloop:
  stx rownum
  txa
  lsr a
  tax
  lda player
  bne start_row_2p
  lda xmax_1p,x
  asl a
  tay
  dey
  lda #1
  bne have_bounds
start_row_2p:
  lda xmin_2p,x
  asl a
  ldy #28
have_bounds:
  ; First tile: A; last tile: Y (inclusive)
  sta xmin
tileloop:
  lda (fieldlo),y
  beq skip_add
  cmp #$03
  bcc add_one
  cmp #$58
  bcc skip_add
add_one:
  inc count_lo
  bne skip_add
  inc count_hi
skip_add:
  dey
  cpy xmin
  bcs tileloop

  ; Go to next row
  lda fieldlo
  adc #32
  sta fieldlo
  bcc :+
  inc fieldhi
:
  ldx rownum
  inx
  cpx #FIELD_HT
  bcc rowloop
  rts
.endproc

;;
; Loads the address of itemdata for item A on the current page
; into $00-$01.
; @param C clear for player 1, set for player 2
; @return Y: item number
.proc seek_to_pageitem_a
  rol a
  tay
  lda form_pageitems,y
  tay
.endproc
.proc seek_to_furni_a
itemdatalo = $00
itemdatahi = $01
  lsr a
  ror a
  ror a
  ror a
  sta itemdatahi
  and #$E0
  ror a
  clc
  adc #<shopitems
  sta itemdatalo
  lda itemdatahi
  and #$0F
  adc #>shopitems
  sta itemdatahi
  rts
.endproc

;; PRESENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Adds the sprites that represent the corners of the shop/stock
; frame to the display list.
.proc shop_frame_corners
  lda form_cur_view+0
  beq noL
  ldx #0
  jsr do_one
noL:
  lda form_cur_view+1
  beq noR
  ldx #16

do_one:
xend = 0
  ldy oam_used
  txa
  clc
  adc #16
  sta xend
loop:
  lda corner_frame_oam,x
  sta OAM,y
  inx
  iny
  cpx xend
  bcc loop
  sty oam_used
noR:
  rts

.pushseg
.segment "RODATA"
top = FIELDTOP_Y - 1 + (FIELD_HT - SHOP_FRAME_HT) * 8
bottom = top + SHOP_FRAME_HT * 8
left1 = 8 + 0 * 8
left2 = 8 + 18 * 8
right1 = left1 + 11 * 8
right2 = left2 + 11 * 8
corner_frame_oam:
  .byte top,    TILE_ROUND_CORNER, $00, left1
  .byte top,    TILE_ROUND_CORNER, $40, right1
  .byte bottom, TILE_FURNI_HALO,   $83, left1
  .byte bottom, TILE_FURNI_HALO,   $C3, right1
  .byte top,    TILE_ROUND_CORNER, $00, left2
  .byte top,    TILE_ROUND_CORNER, $40, right2
  .byte bottom, TILE_FURNI_HALO,   $83, left2
  .byte bottom, TILE_FURNI_HALO,   $C3, right2
.popseg
.endproc

;;
; Draws the sprite for player X's cursor
; provided the shop view is showing.
.proc furnish_draw_cursor
  lda form_cur_view,x
  cmp form_desired_view,x
  beq is_desired_view
  rts
is_desired_view:
  cmp #FRAME_SHOP
  beq is_shop_view
  jmp stock_draw_cursor
is_shop_view:
  lda form_pageitem,x
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y + 8 * (FIELD_HT - SHOP_FRAME_HT) + 8 - 1
  ldy oam_used
  sta OAM,y
  lda #TILE_SHOP_CURSOR
  sta OAM+1,y
  txa
  sta OAM+2,y
  lda shop_cursor_x,x
  sta OAM+3,y
  tya
  clc
  adc #4
  sta oam_used
  
  ; Draw purchased furniture if present
  lda form_buy_timeout,x
  beq no_shopparticle
  asl a
  adc #FIELDTOP_Y+151
  sta $05
  lda form_shopparticletn,x  ; tilenumber
  sta $03
  lda form_shopparticlesz,x  ; size bits
  sta $02
  asl a
  asl a  ; C = wideness
  txa
  rol a
  tay
  lda shopparticle_x,y
  sta $04
  bit $02
  bpl :+
  lda $05
  sec
  sbc #8
  sta $05
:
  jsr draw_furni_as_sprite
  
no_shopparticle:
  rts
.pushseg
.segment "RODATA"
shop_cursor_x:
  .byte FIELDTOP_X + 6, FIELDTOP_X + 18 * 8 + 6
shopparticle_x:
  .byte FIELDTOP_X + 24,        FIELDTOP_X + 16
  .byte FIELDTOP_X + 24 + 18*8, FIELDTOP_X + 16 + 18*8
.popseg
.endproc

;;
; Loads the picture of the furniture corresponding to a particular
; item ID to the first or second half of the transfer buffer.
; A: item id (0-15 or so)
; X: offset (0 or 64)
.proc load_menu_furni
  asl a
  cmp #NUM_FURNIS*2
  bcc :+
    rts
  :
; There's a table of block offsets at the start of menu_furni,
; with each entry big-endian
  tay
  clc
  lda menu_furni+1,y
  adc #<menu_furni
  sta ciSrc
  lda menu_furni+0,y
  adc #>menu_furni
  sta ciSrc+1
un_menu_furni:
; Donut copies and xor many buffers, even if they consist of all 0x00 bytes.
; Here we use a specialy crafted subset that interprets only one block type,
; and writes only once (sometimes twice) to the output buffer.
;
; Assume all blocks are of type '-L-i-C--' in Donut debug notaion.
; (Based on the satistical average of Donut block types for menu_furni16.chr)
;
; That means predict first plane by 0xff and second plane by 0x00,
; xor the decoded second plane onto the first, don't xor on any
; previous buffer, and have a header byte that signifies
; which of the 8 planes are just 8 bytes of 0xff/0x00.
;
; For code simplicity, pb8 planes are *not* encoded upside-down
plane_def = $00
loop_counter = $01
pb8_ctrl = $02
  ldy #$00
  lda (ciSrc), y
    ; Reading a byte from the stream pointer will be pre-increment
    ; So save last increment until routine is done
  sta plane_def

  lda #$100-8
  sta loop_counter
    ; counter is negative so that we can use the
    ; high bits to easily set the V flag.
  plane_loop:
    ; read a bit to determine if next plane is all 0x00/0xff
    lda #$00
    asl plane_def
    bcc pb8_is_all_zero
      iny
      lda (ciSrc), y
      bit loop_counter  ; set V
    pb8_is_all_zero:
    ; if plane is all zero, leave V cleared so that
    ; we don't waste time xoring 8 zeros
    sta pb8_ctrl

    ; set the top value of pb8 to either 0x00 or 0xff
    ; planes 0,2,4,6 = 0xff, planes 1,3,5,7 = 0x00
    lda loop_counter
    and #$01
    bne not_even_plane
      ;,; lda #$01
      lda #$fe  ; xor with 0x01 equals 0xff
      clv       ; first planes don't xor with any plane
    not_even_plane:
    eor #$01

    ; prime the pb8 loop a begin writing to the output buffer
    sec
    rol pb8_ctrl
    pb8_loop:
      ; at this point:
      ; A: previous byte in this plane
      ; C = 0: repeat previous byte
      ; C = 1: read new byte from bitstream
      bcc pb8_use_prev
        iny
        lda (ciSrc), y
      pb8_use_prev:
      sta PB53_outbuf, x
      bvc skip_writing_to_previous_plane
        eor a:PB53_outbuf-8, x
        sta a:PB53_outbuf-8, x
        lda PB53_outbuf, x
      skip_writing_to_previous_plane:
      inx
      asl pb8_ctrl
    bne pb8_loop
    inc loop_counter
  bne plane_loop
  ; add accumulated pointer offset
  sec  ;,; iny   clc
  tya
  adc ciSrc
  sta ciSrc
  bcc :+
    inc ciSrc+1
  :
  rts
rts
.endproc

;;
; Copies a 10x7-tile nametable segment to the transfer buffer.
; @param A:Y source address
; @param enclose_turn player number (0 or 1)
.proc shop_load_nt
srclo = 0
srchi = 1
ormask = 2
  sty srclo
  sta srchi
  lda enclose_turn
  beq :+
  lda #$40
:
  sta ormask
  ldy #SHOP_FRAME_HT * (SHOP_FRAME_WID - 2) - 1
loop:
  lda (srclo),y
  bpl noplayercorrect
  cmp #$B0
  bcs noplayercorrect
  ora ormask
noplayercorrect:
  sta lineImgBuf,y
  dey
  bpl loop
  rts
.endproc

;;
; Loads the tiles for the title of the stock/shop form.
.proc shop_load_heading_tiles
  jsr clearLineImg
  lda #>label1
  ldy #<label1
  ldx #4
  jsr vwfPuts
  lda #>label2
  ldy #<label2
  ldx #4+32
  jsr vwfPuts
  lda #8
  jsr invertTiles
  lda #$0B
  ldy #$08
  jsr docopy
  
  jsr clearLineImg
  lda #8
  jsr invertTiles
  lda #>label1
  ldy #<label1
  ldx #4+64
  jsr vwfPuts
  lda #>label2
  ldy #<label2
  ldx #4+96
  jsr vwfPuts
  lda #$0B
  ldy #$00
docopy:
  ldx nmis
:
  cpx nmis
  beq :-
  jsr copyLineImg
  jmp screen_back_on
.pushseg
.segment "RODATA"
label1: .byte "Stock",0
label2: .byte "Shop",0
.popseg
.endproc

;;
; Draws the name and price of two shop items from
; the list of affordable shop items.
; @param enclose_turn player (0 or 1)
; @param A linenum (0 or 2)
.proc draw_two_shopitems
linenum = $0C
itemdatalo = $00
itemdatahi = $01
namebuf = short_string_buf+7

  sta linenum
  jsr clearLineImg
lineloop:
  ; If after the last affordable item on this page, finish
  ldx enclose_turn
  lda linenum
  cmp form_pagelen,x
  bcs done
  
  ; Load name
  cpx #1
  jsr seek_to_pageitem_a
  ldy #15
  ldx #7
nameloop:
  lda (itemdatalo),y
  sta namebuf,x
  dey
  dex
  bpl nameloop
  lda #0
  sta namebuf+8
  lda linenum
  and #$01
  beq :+
  lda #64
:
  pha
  ora #40
  tax

  ; Write price and name
;  ldy #7  ; after nameloop it's already 7
  lda (itemdatalo),y
  tay
  lda #0
  jsr right_align_digits
  pla
  tax
  lda #>namebuf
  ldy #<namebuf
  jsr vwfPuts

  ; Go to next line
  inc linenum
  lda linenum
  lsr a
  bcs lineloop
done:
  lda #16
  jmp invertTiles
.endproc

;;
; Draws the player's cash balance and the number of the currently
; selected item that the player owns.
; @param enclose_turn player (0 or 1)
.proc shop_draw_cash
  jsr clearLineImg
  
  ; Draw the cash amount
  ldx enclose_turn
  ldy cash_lo,x
  lda cash_hi,x
  ldx #128-24
  jsr right_align_digits

  ; Draw the currency symbol
  tya
  sta 0
  asl a
  asl a
  adc 0
  eor #$7F
  adc #<-3
  tax
  lda #CIO_SIGN
  jsr vwfPutTile
  
  lda #16
  jmp invertTiles
.endproc

.segment "RODATA"
menu_furni:
  .incbin "obj/nes/menu_furni.minid"

shop_nt:
  mbyt "B8B9BABBB4B5B6B70F0F"
  mbyt "0E80818283848586870E"
  mbyt "0E88898A8B8C8D8E8F0E"
  mbyt "0E90919293949596970E"
  mbyt "0E98999A9B9C9D9E9F0E"
  mbyt "0EA0A20E0EA8A9AAAB0E"
  mbyt "0EA1A3A4A5ACADAEAF0E"

stock_nt:
  mbyt "B0B1B2B3BCBDBEBF0F0F"
  mbyt "0E80828486888A8C8E0E"
  mbyt "0E81838587898B8D8F0E"
  mbyt "0EA0A1A2A3A4A5A6A70E"
  mbyt "0E90929496989A9C9E0E"
  mbyt "0E91939597999B9D9F0E"
  mbyt "0EA8A9AAABACADAEAF0E"

