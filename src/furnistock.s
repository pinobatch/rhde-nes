;
; RHDE furnish phase: stock window and furni placement/pickup
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.h"
.include "ram.h"
.import fill_pageitems_done, shopitems

PICKUP_GOES_TO_PLACEMENT = 1

.segment "BSS"
player_inv: .res 2 * NUM_FURNIS

.segment "CODE"
.proc stock_move_cursor
  lda new_keys,x
  bpl notA

  ; A: Switch to placing the selected item
  ; B+A: Sell one of the selected item
  ; But first make sure the item exists
  lda form_pageitem,x
  cmp form_pagelen,x
  bcs bad_item
  cpx #1  ; Look up the item at this position in the menu
  rol a
  tay
  lda form_pageitems,y
  cmp #NUM_FURNIS
  bcs bad_item
  sta placement_item,x
  cpx #1  ; Ensure that the player has at least one of these to place
  rol a
  tay
  lda player_inv,y
  beq bad_item
  
  ; Decide whether it was A or B+A
  lda cur_keys,x
  and #KEY_B
  beq not_selling
  
  ; Remove one from inventory
  ; (no DEC a,X)
  lda player_inv,y
  sec
  sbc #1
  sta player_inv,y
  
  ; And add 1/4 (rounded up) of the price of the item
  tya
  lsr a
  jsr seek_to_furni_a
  ldy #SHOPITEMS_PRICE
  lda (0),y
  lsr a
  lsr a  ; carry = rounding
  adc cash_lo,x
  sta cash_lo,x
  bcc :+
    inc cash_hi,x
  :

  ; Finally trigger a sound effect and a form update
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x
  lda #SFX_SCANBEEP
  jmp start_sound

not_selling:
  ; lda #0  ; already 0 from the B button test
  sta form_desired_view,x
bad_item:
  rts
notA:
  lsr a
  bcc notRight

  ; Right: Go to next item.  If at right column, or at last item
  ; on first row of page, go to shop.  
  lda form_pageitem,x
  adc #1-1
  cmp form_pagelen,x
  bcc right_notpast
  sbc #4
  bcc go_to_shop
right_notpast:
  sta form_pageitem,x
  and #$03
  beq go_to_shop
  rts
go_to_shop:
  lda #SFX_TURN_PAGE
  jsr start_sound
  jmp setup_shop_view
notRight:

  lsr a
  bcc notLeft

  ; Left: Go to previous column  
  lda form_pageitem,x
  and #$03
  beq knownrts1
  dec form_pageitem,x
knownrts1:
  rts
notLeft:
  lsr a
  bcc notDown
  
  ; Down: Go to next row or, if this page is full, next page
  lda form_pagelen,x
  beq down_have_pageitem  ; Don't try moving if stock is empty
  lda form_pageitem,x
  clc
  adc #4
  cmp #STOCK_PAGELEN
  bcs need_next_page
pageitem_a_or_last:
  cmp form_pagelen,x  ; If the next row is shorter, move to the left
  bcc :+
  lda form_pagelen,x
  sbc #1
down_have_pageitem: 
  sta form_pageitem,x
  rts
need_next_page:
  lda form_pagelen,x  ; Check if page full
  cmp #STOCK_PAGELEN
  bcc knownrts1
  lda form_pagenum,x  ; Check if last possible page
  cmp #(NUM_FURNIS - 1)/STOCK_PAGELEN
  bcs knownrts1
  inc form_pagenum,x  ; Try to go to next page
  lda form_pagenum,x
  asl a
  asl a
  asl a
  tay
  jsr find_owned_stockitems
  cpx #2
  bcc need_prev_page
  ldx cur_turn
  lda form_pageitem,x  ; And move to the top of the page
  and #$03
  cmp form_pagelen,x
  bcc :+
  lda form_pagelen,x
  sbc #1
:  
  sta form_pageitem,x
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x
  rts
notDown:
  lsr a
  bcc notUp

  ; Up: Go to previous row, or go to previous page, or go to pickup
  lda form_pageitem,x
  sec
  sbc #4
  bcs pageitem_a_or_last
need_prev_page:
  
  lda #DIRTY_FORM
  ora side_dirty,x
  sta side_dirty,x
  lda form_pagenum,x
  beq switch_to_pickup
  dec form_pagenum,x 
  lda form_pagenum,x
  asl a
  asl a
  asl a
  tay
  jsr find_owned_stockitems
  ; FIXME: WTF is this supposed to do?
  txa
  and #$0F
  tax
  lda form_pageitem,x
  ora #$04
  bne pageitem_a_or_last
notUp:
  rts
switch_to_pickup:

  ; Up from first page, or Down from empty page:
  ; Go to placement with empty furniture
  lda #$80
  sta placement_item,x
  asl a
  sta form_desired_view,x
  rts
.endproc


.proc setup_stock_view
  ldx cur_turn
  lda #FRAME_STOCK
  sta form_desired_view,x
  lda #0
  sta form_pagenum,x
  sta form_pageitem,x
  tay
  ; fall through to find_owned_stockitems
.endproc
;;
; Finds the first eight items with positive quantity belonging to
; player X, skipping the first Y.  Pass Y=$FF and invert skipitems
; to see how many owned items there are.
; @param X player number (0 or 1)
; @param Y number of items to skip (usually 0, 8, or 255)
; @return X = number of items found times 2, plus player number
;         Y = remaining skips
;         form_pageitems: beginning filled with owned items
;         form_pagelen: number of items found (0 to 4)
.proc find_owned_stockitems
skipitems = $05
  sty skipitems
  txa
  tay
loop:
  lda player_inv,y
  beq next_item
  lda skipitems
  beq add_item
  dec skipitems
  jmp next_item
add_item:
  tya
  lsr a
  sta form_pageitems,x
  inx
  inx
  cpx #2 * STOCK_PAGELEN
  bcs done
next_item:
  iny
  iny
  cpy #NUM_FURNIS * 2
  bcc loop
done:
  jmp fill_pageitems_done
.endproc

.proc placement_move_cursor
itemdata = $00
  lda placement_item,x
  bpl is_an_actual_item
  lda #0
  beq have_itemsize
is_an_actual_item:
  jsr seek_to_furni_a
  ldy #SHOPITEMS_SIZE
  lda (itemdata),y
have_itemsize:
  ldy new_keys,x
  bmi furnish_place_or_pickup
notA:
  cpy #KEY_B
  bcc notB

  ; Press B in pickup: Exit
  lda placement_item,x
  bmi holdBexit
  ; Press B with piece that can't be rotated: Exit
  ldy #SHOPITEMS_ROTNEXT
  cmp (itemdata),y
  beq holdBexit
  ; Press B with piece that can be rotated: Rotate piece
  lda (itemdata),y
  sta placement_item,x
  jsr autorepeat  ; allow the B press to get into the repeat buffer
  lda #SFX_TURN
  jmp start_sound
notB:
  asl a
  rol a
  rol a
  and #$03
  tay
  lda itemsize_to_cur_piece_lo,y
  sta cur_piece_lo,x
  lda #$81  ; center on, rotation off
  sta cur_piece_hi,x
  lda das_keys,x
  and #KEY_B|KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT
  sta das_keys,x
  jsr autorepeat
  lda new_keys,x
  and #KEY_B
  bne holdBexit
  jmp build_handle_shifting
holdBexit:
  ; Hold B: Return to stock view
  jmp setup_stock_view
.endproc

.pushseg
.segment "RODATA"
; Furni movement in placement piggybacks on piece movement in build.
; It sets cur_piece_lo appropriately for the furniture shape.
; . . .  Small furniture
; . % .  $00 -> $00
; . . .
; . . .  Wide furniture
; . % %  $40 -> $01
; . . .
; . . .  Tall furniture
; . % .  $80 -> $40
; . % .
; . . .  Large furniture
; . % %  $C0 -> $C1
; . % %

itemsize_to_cur_piece_lo:
  .byte $00, $01, $40, $C1
.popseg

;;
; Handles A button
; @param 00-01 pointer to piece record for placement_item
; @param X player number (0-1)
.proc furnish_place_or_pickup
itemdata = $00
  ; Seek to area near cursor (get_piece_top_left_corner)
  jsr get_piece_top_left_corner
  lda placement_item,x
  bmi :+
  jmp furnish_place_piece
:
  ; Handle pickup
  jsr find_furni_at_cursor
  bpl found_furni
invalid_furni:
  rts
found_furni:
  rol a  ; $20: move up; $02: move left; $01: use outdoor tile
  cmp #$20
  bcc :+
  dec cursor_y,x
  and #$03
:
  cmp #$02
  bcc :+
  dec cursor_x,x
:
  tya
  cmp #NUM_ALL_FURNIS
  bcs invalid_furni

  ; We have a valid furniture ID. Erase the furniture.
  pha
  ldy #SHOPITEMS_SIZE
  lda (itemdata),y
  jsr erase_furni
  pla

  ; Add it to the player's inventory.
  jsr find_furni_main_rot
  .if ::PICKUP_GOES_TO_PLACEMENT
    sta placement_item,x
  .endif
  cpx #1
  rol a
  tax
  inc player_inv,x
  and #1
  tax
  .if ::PICKUP_GOES_TO_PLACEMENT
    rts
  .else
    lda #SFX_TURN
    jsr start_sound
    jmp setup_stock_view
  .endif
.endproc

;;
; Finds the primary rotation of a furniture ID.  (NUM_FURNIS is the
; boundary between primary rotations and alternate rotations.)
; @param A furniture ID
; @return A = ID of primary rotation; C = 0 for valid, 1 for not
.proc find_furni_main_rot
itemdata = $00
  cmp #NUM_ALL_FURNIS
  bcs valid_furni
  cmp #NUM_FURNIS
  bcc valid_furni
  jsr seek_to_furni_a
  asl a
  ldy #SHOPITEMS_ROTNEXT
  lda (itemdata),y
  jmp find_furni_main_rot
valid_furni:
  rts
.endproc

;;
; @param X player whose (cursor_x, cursor_y) is used
; @param A footprint of furni (7: tall, 6: wide, 5: indoor)
.proc erase_furni
floortile = $02
ymax = $03
furnisize = $04
furniid = $05

  ; Choose a tile for filling the area
  sta furnisize
  and #$20
  cmp #$20  ; C = 1 if outside else 0
  lda #TILE_FLOOR
  bcs :+
  lda cursor_y,x
  asl a
  eor cursor_x,x
  and #$03
  ora #TILE_GRASS
:
  sta floortile
  
  ; Calculate Y limit
  lda furnisize
  cmp #$80  ; C = 1 if tall else 0
  ldy #64
  bcc :+
  ldy #96
:
  sty ymax

  ; Get playfield address of new top left corner
  jsr get_piece_top_left_corner
  ldy #33
rowloop:
  lda floortile
  sta (fieldlo),y
  bit furnisize
  bvc not2wide  ; V = 1 if wide else 0
  cmp #TILE_GRASS
  bcc :+
  adc #$00
  and #$03
  ora #$04
:
  iny
  sta (fieldlo),y
  dey
not2wide:
  lda floortile
  cmp #TILE_FLOOR
  bcc :+
  eor #$02
  clc
:
  tya
  adc #32
  tay
  cpy ymax
  bcc rowloop
  jmp update_around_cursor
.endproc

;;
; Determines whether the player can place a piece here.
; @param itemdata pointer to item's data
; @param fieldlo top left corner
; @return itemsizedata, itembasetile, furni_w, furni_h populated
.proc furnish_can_place_piece
itemdata = $00
itemsizedata = $02
itembasetile = $03
furni_w = $04
furni_h = $05
tilerangestart = $06
tilerangelen = $07
xleft = $08
yleft = $09
  ldy #1
  sty furni_w
  sty furni_h
  ldy #TILE_GRASS
  sty tilerangestart  ; grass tile start
  sty tilerangelen    ; grass tile count
  ldy #SHOPITEMS_TILE
  lda (itemdata),y
  sta itembasetile
  dey  ; now equal to SHOPITEMS_SIZE
  lda (itemdata),y
  sta itemsizedata

  ; Bit 7: tall
  asl a
  bcc :+
  asl furni_h
:
  ; Bit 6: wide
  asl a
  bcc :+
  asl furni_w
:
  ; Bit 5: indoor
  asl a
  bcc :+
  lsr tilerangelen
  ldy #1
  sty tilerangestart
:
  ; Check that tiles under furni are within range
  lda furni_h
  sta yleft
chkrowloop:
  ldy yleft
  lda furni_w
  sta xleft
  ora ym1times32,y
  tay
chktileloop:
  lda (fieldlo),y
  sec
  sbc tilerangestart
  cmp tilerangelen
  bcs bail
  dey
  dec xleft
  bne chktileloop
  dec yleft
  bne chkrowloop
bail:
  rts
.endproc

;;
; precondition: X = cur_turn = player number, fieldlo points at piece
; top left corner, $00-$01 points at item id; A = item id
.proc furnish_place_piece
itemdata = $00
itembasetile = $03
furni_w = $04
furni_h = $05
xleft = $08
yleft = $09

  pha
  jsr find_furni_main_rot
  cpx #1
  rol a
  tay
  lda player_inv,y
  bne yes_player_has_it
  pla
  jmp leave_placement_mode
yes_player_has_it:

  ; Determine width, height, and indoorness of furni type
  pla
  jsr furnish_can_place_piece
  bcc not_blocked
  rts
not_blocked:

  ; Write furni to region

  ldy #SHOPITEMS_TILE
  lda (itemdata),y
  cmp #TILE_FLOWERS
  bne not_flower
  lda cursor_x,x
  eor cursor_y,x
  and #1
  eor #TILE_FLOWERS
not_flower:
  sta itembasetile
  lda furni_h
  sta yleft
wrrowloop:
  ldy yleft
  lda tilenumaddy,y
  clc
  adc itembasetile
  tax
  lda furni_w
  sta xleft
  lda ym1times32,y
  tay
  iny
wrtileloop:
  txa
  sta (fieldlo),y
  iny
  inx
  dec xleft
  bne wrtileloop
  dec yleft
  bne wrrowloop
nowrite:

  ; And remove it from player's inventory
  lda #SFX_PLACE
  jsr start_sound
  ldx cur_turn
  jsr update_around_cursor
  lda placement_item,x
  jsr find_furni_main_rot
  cpx #1
  rol a
  tax
  dec player_inv,x
  bne still_have_some
leave_placement_mode:
  ldx cur_turn
  lda #FRAME_STOCK
  sta form_desired_view,x
still_have_some:
  ldx cur_turn
  rts
.pushseg
.segment "RODATA"
tilenumaddy = * - 1
  .byte 0, 16
.popseg
.endproc

.segment "RODATA"
ym1times32 = * - 1
  .byte 32, 64
.segment "CODE"

;;
; Finds which furniture item the cursor is on.
; @param X which player's cursor to use
;        fieldlo: points above and to left of cursor
; @return Y: furniture id found, or $FF if not found
;         A: cursor offset from top left (YYYYXXXX)
;         N: PL if found, or MI if not found
;         C: 0 for outside or 1 for inside
;         $00-$01: pointer to furniture struct
.proc find_furni_at_cursor
  jsr get_piece_top_left_corner
  ldy #33
  lda (fieldlo),y
  ; fall through
.endproc

;;
; Finds which furniture item the tile is part of.
; @param A tile number ($00-$7F)
; @return Y: furni id; A: offset from base tile number (YYYYXXXX);
; flags: BPL if valid furni
.proc find_furni_tile_a
itemdatalo = $00
itemdatahi = $01
tilehere = $02
furniid = $03
tilediff = $04
furnisize = $05

  sta tilehere
  lda #>shopitems
  sta itemdatahi
  lda #<shopitems
  sta itemdatalo
  ldy #0
  
  ; For each item: Subtract tile under cursor and compare the
  ; difference to the furni's size
furniscan_loop:
  sty furniid
  ldy #SHOPITEMS_TILE
  
  ; Find where the tile under the cursor is compared to the furni's
  ; origin
  lda tilehere
  sec
  sbc (itemdatalo),y
  sta tilediff
  and #$EE
  bne furniscan_continue
  dey
  lda (itemdatalo),y
  and #$E0  ; 7: tall, 6: wide, 5: indoor
  sta furnisize
  asl a
  rol a
  rol a
  tay
  lda tilediff
  and pickup_size_reject_mask,y
  beq furniscan_found
  
furniscan_continue:
  clc
  lda #16
  adc itemdatalo
  sta itemdatalo
  bcc :+
  inc itemdatahi
:
  ldy furniid
  iny
  cpy #NUM_ALL_FURNIS
  bcc furniscan_loop

  ; At this point we've scanned the entire furni table without
  ; finding a match.
  ldy #$FF
  rts

furniscan_found:
  lda tilediff
  ldy furniid
  rts
.pushseg
.segment "RODATA"
pickup_size_reject_mask:
  .byte $FF, $FE, $EF, $EE
.popseg
.endproc

;; PRESENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Writes 3-digit number AA at position X (14px wide).
.proc right_align_3digits
highdigits = $00
topbits = $01
penpos = $0A
  stx penpos
  jsr bcd8bit
  ora #'0'
  pha

  lda #0
  sta topbits
  lda highdigits
  lsr a
  lsr a
  lsr a
  lsr a
  jsr possdig
  lda highdigits
  and #$0F
  jsr possdig
  pla
  ldx penpos
  jmp vwfPutTile
possdig:
  ora topbits
  beq nodig
  ora #'0'
  ldx penpos
  jsr vwfPutTile
  lda #'0'
  sta topbits
nodig:
  lda penpos
  clc
  adc #5
  sta penpos
  rts
.endproc

;;
; Draws the count of owned items on this page.
; @param enclose_turn player (0 or 1)
.proc stock_draw_counts
itemoffset = $0B
  jsr clearLineImg
  ldx enclose_turn
  lda form_pagelen,x
  beq none
  sec
  sbc #1
  cpx #1
  rol a
loop:
  sta itemoffset
  tay
  and #$0E
  asl a
  sec
  rol a
  asl a
  tax
  lda form_pageitems,y
  cmp #NUM_FURNIS
  bcs invalid
  asl a
  ora enclose_turn
  tay
  lda player_inv,y
  jsr right_align_3digits
invalid:
  lda itemoffset
  sec
  sbc #2
  bcs loop
none:
  ldx itemoffset
  lda form_pagelen,x
  cmp #5
  bcs no_pickup_notice
  lda #>pickup_notice_msg
  ldy #<pickup_notice_msg
  ldx #64
  jsr vwfPuts
no_pickup_notice:
  lda #16
  jmp invertTiles
.pushseg
.segment "RODATA"
pickup_notice_msg:
  .byte "A:Put ",$84,":Pickup",0
.popseg
.endproc

;;
; @param X, cur_turn player to draw
; @param A FRAME_STOCK for stock, other for placement
.proc stock_draw_cursor
  cmp #FRAME_STOCK
  bne placement_draw_cursor
  lda form_pageitem,x
  cmp #4
  lda #FIELDTOP_Y + 8 * (FIELD_HT - SHOP_FRAME_HT) + 8 + 16 - 1
  bcc :+
  lda #FIELDTOP_Y + 8 * (FIELD_HT - SHOP_FRAME_HT) + 8 + 16 + 24 - 1
:
  ldy oam_used
  sta OAM,y
  lda #TILE_SHOP_CURSOR
  sta OAM+1,y
  txa
  sta OAM+2,y
  lda form_pageitem,x
  and #$03
  asl a
  asl a
  asl a
  asl a
  clc
  adc stock_cursor_x,x
  sta OAM+3,y
  tya
  clc
  adc #4
  sta oam_used
  rts
.pushseg
stock_cursor_x:
  .byte FIELDTOP_X + 15, FIELDTOP_X + 18 * 8 + 15
.popseg
.endproc


.proc placement_draw_cursor
  
  ; To draw the item, we'll need to look up its base tile and its
  ; size (1x1, 2x1, 1x2, or 2x2).
itemdata = $00
itemsize = $02  ; BIT BMI: tall; BIT BVS: wide
itembasetile = $03
xbase = $04
ybase = $05
halotype = $08

  lda placement_item,x
  bpl is_real_item

  ; Draw the pickup cursor
  jsr find_furni_at_cursor
  cpy #$80
  lda #0
  rol a
  sta halotype
  lda #TILE_HANDTRUCK
  sta itembasetile
  lda #0
  sta itemsize
  beq draw_this_item

is_real_item:
  jsr seek_to_furni_a
  jsr get_piece_top_left_corner
  jsr furnish_can_place_piece
  lda #0
  rol a
  sta halotype

  ; Small outdoor furniture tiles are at a different place
  ; in the sprite pattern table
  lda itembasetile
  cmp #$50
  bcs draw_this_item
  cmp #$30  ; $28->$50 fence; $38->$51 flower
  lda #$50 >> 1
  rol a
  sta itembasetile
draw_this_item:

  ; Draw the item
  lda cursor_y,x
  asl a
  asl a
  asl a
  adc #FIELDTOP_Y-1
  sta ybase
  lda cursor_x,x
  asl a
  asl a
  asl a
  adc #FIELDTOP_X
  sta xbase
  jsr draw_furni_as_sprite

  ; Alternate frames for the halo
  txa
  eor nmis
  lsr a
  bcs no_halo

  ; Don't draw a halo under the pickup cursor
  lda itembasetile
  cmp #$20
  bcc no_halo
draw_halo:

  ; At this point, (xbase, ybase) is at the top left of the last
  ; row of tiles in the furni.  We need to move it different
  ; distances for different furni widths but keep it the same for
  ; normal vs. tall furniture.
  ldy oam_used
  tya
  clc
  adc #16
  sta oam_used
  lda #<-3  ; center under furni's visual center, not true center
  clc
  adc ybase
  sta OAM+0,y
  sta OAM+4,y
  clc
  adc #8
  sta OAM+8,y
  sta OAM+12,y

  lda #<-4  ; Center halo horizontally under furni
  bit itemsize
  bvc :+
  lda #<-0
:
  clc
  adc xbase
  sta OAM+3,y
  sta OAM+11,y
  clc
  adc #8
  sta OAM+7,y
  sta OAM+15,y
  lda #TILE_FURNI_HALO
  eor halotype
  sta OAM+1,y
  sta OAM+5,y
  sta OAM+9,y
  sta OAM+13,y
  txa
  sta OAM+2,y
  eor #$40
  sta OAM+6,y
  eor #$C0
  sta OAM+10,y
  eor #$40
  sta OAM+14,y
no_halo:
  rts
.endproc

;;
; Draws a rectangular sprite.  Tile indices increase by $01 to the
; right or $10 down.
; @param itemsize $80 for tall, $40 for wide, or both or neither
; @param itembasetile tile at top right corner
; @param xbase starting X coordinate
; @param ybase starting Y coordinate
; @param X player number (0 or 1)
.proc draw_furni_as_sprite
itembasetile = $03
itemsize = $02
xbase = $04
ybase = $05

xleft = $00
yleft = $01
itemcurtile = $06
xcur = $07

  lda #1
  bit itemsize
  bpl :+
  asl a
:
  sta yleft
  ldy oam_used
rowloop:
  lda xbase
  sta xcur
  lda itembasetile
  sta itemcurtile
  lda #1
  bit itemsize
  bvc :+
  asl a
:
  sta xleft
tileloop:
  lda ybase
  sta OAM,y
  iny
  lda itemcurtile
  inc itemcurtile
  sta OAM,y
  iny
  txa
  and #$01
  sta OAM,y
  iny
  lda xcur
  sta OAM,y
  iny
  clc
  adc #8
  sta xcur
  dec xleft
  bne tileloop
  dec yleft
  beq furni_done
  lda #16
  clc
  adc itembasetile
  sta itembasetile
  lda #8
  clc
  adc ybase
  sta ybase
  jmp rowloop
furni_done:
  sty oam_used
  rts
.endproc


;;
; Draws the pictures of two stock items from the list of
; owned stock items.
; @param enclose_turn player (0 or 1)
; @param A linenum (0, 2, 4, or 6)
.proc draw_two_stockitems
  pha
  jsr clear_8_tiles
  pla
  pha
  ldx #0
  jsr draw_one_stockitem
  pla
  ora #$01
  ldx #64
draw_one_stockitem:
  ldy enclose_turn
  cmp form_pagelen,y
  bcc item_is_on_page
  rts
item_is_on_page:
  cpy #1
  rol a
  tay
  lda form_pageitems,y
  jmp load_menu_furni
.endproc

