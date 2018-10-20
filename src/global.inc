;
; RHDE constants, public fields, and public methods
; Copyright 2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.global OAM
FIELD_HT = 24
ROAD_HT = FIELD_HT / 2
SHOP_FRAME_WID = 12
SHOP_FRAME_HT = 7  ; not including last row that overlaps screen border

; Single tiles
TILE_BAD_GRASS = $00
TILE_FLOOR = $01
TILE_DARK_FLOOR = $03
TILE_FRAME_TOP = $08
TILE_FRAME_LEFT = $09
TILE_FRAME_RIGHT = $0A
TILE_FRAME_BOTTOM = $0B
TILE_WHITE = $0E
TILE_BLACK = $0F
TILE_OV_BLOCK = $10
TILE_OV_STRAY_BLOCK = $11
TILE_OV_DOOR_WE = $12
TILE_OV_DOOR_NS = $13
TILE_TLBORDER = $20
TILE_TBORDER = $21
TILE_TLINLET = $22
TILE_TRINLET = $23
TILE_TRBORDER = $24
TILE_LBORDER = $25
TILE_RBORDER = $35
TILE_BLBORDER = $30
TILE_BBORDER = $31
TILE_BLINLET = $32
TILE_BRINLET = $33
TILE_BRBORDER = $34
TILE_FENCE = $38
TILE_RUBBLE = $39
TILE_BED = $60
CONNBLKS_DOORWE = CONNBLKS_ROW + 16
CONNBLKS_DOORNS = CONNBLKS_DOORWE + 1

; Tile ranges
TILE_GRASS = $04      ; through $07
TILE_ROADS = $26
TILE_FLOWERS = $28
CONNBLKS_ROW = $40
CONNBLKS_E   = $01
CONNBLKS_W   = $03
CONNBLKS_S   = $04
CONNBLKS_N   = $0C

FIELDTOP_X = 8
FIELDTOP_Y = 20

; 1st half walls, 2nd half doors
TILE_ENCLOSE_BASE = TILE_OV_BLOCK
TILE_ENCLOSE_COUNT = TILE_OV_DOOR_NS + 1 - TILE_OV_BLOCK

; In-game sprite tiles
TILE_PREVIEW_BASE = $10
TILE_PATH_DOT = $17
TILE_SHOP_CURSOR = $18
TILE_DROPPED_FURNI = $19
TILE_A_BUTTON = $1A
TILE_B_BUTTON = $1B
TILE_ROUND_CORNER = $1C
TILE_FURNI_HALO = $1D
TILE_PIECE_CURSOR = $1E
TILE_PIECE_CENTER = $1F
TILE_SWUNG_BAT = $20
TILE_PLAYER_MARKER = $38
TILE_LOCK_POPUP = $3A
TILE_HANDTRUCK = $3B
TILE_HPMARKER = $3C

; main.s
.globalzp NUM_ROUNDS_TO_WIN
.globalzp oam_used, cur_turn, cursor_x, cursor_y, nmis
.globalzp time_subtenths, time_tenths, time_seconds, phase_seconds
.globalzp cash_lo, cash_hi, player_race, debughex, hra_wins
.global update_game_time, countdown_logic, delay_500, delay_y_seconds
.global load_blk_chr, draw_debughex

; pads.s
.global read_pads, autorepeat
.globalzp cur_keys, new_keys, das_keys, das_timer

; ppuclear.s
.global ppu_zero_nt, ppu_clear_nt, ppu_clear_oam, ppu_screen_on

; flood.s
DIRTY_SIDE       = 1<<0
DIRTY_ENCLOSE    = 1<<1
DIRTY_RM_ENDS    = 1<<2
DIRTY_DOORLESS   = 1<<3
.global side_dirty, enclose_state, enclose_turn, enclose_play_sfx
.global fielddata, floodscratch, walldata_l, walldata_r
.global xmin_2p, xmax_1p
.global enclose_run_step, interactive_encloop

; bg.s
COLOR_GOODGRASS = $1A
COLOR_BADGRASS = $19
.globalzp copy_region_width, copy_region_left
.globalzp fullht_min_col, fullht_max_col
.globalzp fieldlo, fieldhi, player_grass_color
.global init_playfield, make_road, place_initial_houses
.global make_road_border
.global update_around_cursor, update_around_column_y
.global shop_choose_redraw, build_choose_redraw, bottom_row_choose_redraw
.global bgup_vsync, screen_back_on
.global seek_fielddata_row_a, seek_fielddata_row_y, tile_to_addr
.global shop_draw_frame, shop_erase_frame_bottom, shop_copy_nt
.global copy_field_cols, clear_vwf_rows, clear_8_tiles, copy_8_tiles

; alert.s
LF = $0A
.global short_string_buf, right_align_digits
.global prepare_alertbox, remove_alertbox
.global alert_doors, alert_rent_result
.global alert_furnish, alert_furnish_result
.global alert_battle
.global alert_build

; makedoors.s
.global doors_phase, draw_player_x_cursor
.global init_breakdoors, inc_breakdoors

; shopping.s
SHOP_PAGELEN = 4
STOCK_PAGELEN = 8
FRAME_NONE = 0
FRAME_BLANK = 1
FRAME_SHOP = 2
FRAME_STOCK = 3
DIRTY_FORM       = 1<<4
DIRTY_FORMEND    = 1<<5

; NUM_ALL_FURNIS includes rotated versions
; It has to be set here because it has to be "constant" in furnistock.s
NUM_FURNIS = 17
NUM_FURNIS_NONWEAPON = 16
.globalzp item_bed, item_table, item_chair, item_rug
.globalzp item_sofa, item_bookcase, item_fridge, item_oven
.globalzp item_sink, item_toilet, item_bathtub, item_trashcan
.globalzp item_ficus
.globalzp item_silo, item_siloused, item_bat
.globalzp NUM_ALL_FURNIS
.global furnish_phase, setup_shop_view
.global count_rent_tiles, shop_frame_corners, shop_load_heading_tiles
.global draw_two_shopitems, shop_draw_cash
.global load_menu_furni
.global shop_load_nt, stock_nt, shop_nt
.global seek_to_furni_a

; Borrow the combat phase's unit movement state and the flood fill
; CA's state for shopping-related variables.  form_pagenum and
; form_pageitem save the menu cursor separately from the placement
; cursor (cursor_x and cursor_y).
form_pageitems = floodscratch + 0
form_pageitem = unitzp24 + 0  ; item within page
form_pagelen = unitzp24 + 2  ; number of present items
form_pagenum = unitzp24 + 4  ; number of 4-item (shop) or 8-item (stock) page
form_cur_view = unitzp24 + 6
form_desired_view = unitzp24 + 8
placement_item = unitzp24 + 10
form_buy_timeout = unitzp24 + 12
form_shopparticletn = unitzp24 + 14
form_shopparticlesz = unitzp24 + 16

; furnistock.s
SHOPITEMS_ROTNEXT = $04
SHOPITEMS_SIZE = $05
SHOPITEMS_TILE = $06
SHOPITEMS_PRICE = $07
SHOPITEMS_TALL = $80
SHOPITEMS_WIDE = $40
SHOPITEMS_INDOOR = $20
.global player_inv
.global setup_stock_view, stock_move_cursor, placement_move_cursor
.global draw_two_stockitems, stock_draw_counts, stock_draw_cursor
.global find_furni_tile_a, find_furni_main_rot, erase_furni
.global draw_furni_as_sprite

; battle.s
PASSABILITY_YES = 0
PASSABILITY_NO = 1
PASSABILITY_TEAM = 2
.globalzp unitzp24
.global battle_phase, is_tile_passable
.global furni_in_front_of_unit, is_unit_by_cannon
.globalzp player_cannon_x, player_cannon_y

PLAYER_CONTROL_UNIT = 0
PLAYER_CONTROL_CANNON = 2
MAX_UNITS = 3

; missiles.s
.global init_cannons, control_cannon, cannon_cooldown
.global move_cannon_missiles, any_cannon_missiles_left
.global draw_cannon_cursor, draw_cannon_missiles

; droppedfurni.s
MAX_DROPPED_FURNI = 8
.global init_dropped_furni, add_dropped_furni, draw_dropped_furni
.global query_dropped_furni, take_dropped_furni, cleanup_dropped_furni

; build.s
.global build_phase, build_draw_timer, get_piece_top_left_corner
.global build_handle_shifting
.globalzp cur_piece_lo, cur_piece_hi, next_piece

; connect.s
.global connect_field_start, connect_field_continue
.global disconnect_field_start, disconnect_field_continue
.global pickup_outdoor_furni, pickup_outdoor_furni_continue

; bcd.s
.global bcd8bit
.global bcdConvert
.globalzp bcdNum, bcdResult

; random.s
.global crc16_update, rand_crc, lru4_init, lru4_deal_piece
.globalzp CRCLO, CRCHI

; unpb53.s
.importzp ciSrc, ciDst, ciBufStart, ciBufEnd
.import PB53_outbuf
;.import unpb53_some

; donut.s
.import donut_decompress_block

; math.s
.import getAngle
.import missileSine, missileCosine

; vwfdraw.s
CIO_SIGN = $80
COPR_SIGN = $81
.global clearLineImg, invertTiles, copyLineImg, lineImgBuf
.global vwfPutTile, vwfStrWidth0, vwfPuts, vwfPuts0

; title.s
.globalzp rounds_to_win
.global title_screen

; racesel.s
.global racesel

; hra.s
.global rate_furni

; musicseq.s
SFX_TURN = 0
SFX_MOVE = 1
SFX_PLACE = 2
SFX_ENCLOSE = 3
SFX_COUNTDOWN = 4
SFX_TURN_PAGE = 5
SFX_LAUNCH = 6
SFX_EXPLODE = 8
SFX_SCANBEEP = 9
MUSIC_BUILD = 0
MUSIC_IRONIC_BATTLE = 1

; sound
.global getTVSystem, pently_init, pently_start_sound, pently_update
.global init_music, pently_stop_music, resume_music, pently_play_note
.globalzp tvSystem
