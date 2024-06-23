#!/usr/bin/env python3
from __future__ import with_statement, division
import pygame as pg

def pattern_fill(sz, src):
    """Fill a surface with a tesselation of a rectangular pattern.

If sz is a pygame.Surface, it is filled.  Otherwise, it must be a
(width, height) tuple, and a new Surface is created.

"""
    if isinstance(sz, pg.Surface):
        dst, sz = sz, sz.get_size()
    else:
        dst = pygame.Surface(sz)
    dw, dh = sz
    sw, sh = src.get_size()
    for y in range(0, dh, sh):
        for x in range(0, dw, sw):
            dst.blit(src, (x, y))
    return dst

def imshow(src, display=None):
    display = display or pg.display
    screen = display.set_mode(src.get_size())
    screen.blit(src, (0, 0))
    pg.display.flip()
    done = False
    clk = pg.time.Clock()
    while not done:
        clk.tick(30)
        for event in pg.event.get():
            if event.type == pg.KEYDOWN:
                done = event
            if event.type == pg.QUIT:
                done = event

def render_frame(dst, t, grass_bg, walkcyc):
    pattern_fill(dst, grass_bg)
    sx_off = t % 4 * 8
    oam = []  # list of src_x, src_y, dst_x, dst_y tuples
    red = (204, 51, 51)
    blue = (0, 102, 255)
    oam.append((8 + sx_off, 8, 16, 112 - 2 * t, red))
    oam.append((8 + sx_off, 24, 2 * t, 32, red))
    oam.append((8 + sx_off, 40, 48, 48 + 2 * t, red))
    oam.append((48 + sx_off, 8, 80, 48 - 2 * t, blue))
    oam.append((48 + sx_off, 24, 64 + 2 * t, 96, blue))
    oam.append((48 + sx_off, 40, 112, 112 + 2 * t, blue))
    for (src_x, src_y, dst_x, dst_y, color) in oam:
        dst_x, dst_y = dst_x % 128, dst_y % 128
        for y in (dst_y - 128, dst_y):
            if y <= -8:
                continue
            for x in (dst_x - 128, dst_x):
                if x <= -8:
                    continue
                walkcyc.set_palette_at(2, color)
                dst.blit(walkcyc, (x, y), (src_x, src_y, 8, 8))

def main():
    grass_bg = pg.image.load("../docs/grass_bg.png")
    walkcyc = pg.image.load("walk2013_cels.png")
    walkcyc.set_colorkey(0)
    dst = pg.Surface((128, 128))
    t = 0
    screen = pg.display.set_mode((512, 512))
    done = False
    clk = pg.time.Clock()
    
    with open("walk2013_out.raw", "wb") as outfp:
        for t in range(0, 64):
            render_frame(dst, t, grass_bg, walkcyc)
            outfp.write(pg.image.tostring(dst, 'RGB'))
    while not done:
        for event in pg.event.get():
            if event.type == pg.KEYDOWN:
                done = event
            if event.type == pg.QUIT:
                done = event
        t += 1
        render_frame(dst, t, grass_bg, walkcyc)
        pg.transform.scale(dst, screen.get_size(), screen)
        pg.display.flip()
        clk.tick(5)

if __name__=='__main__':
    pg.init()
    try:
        main()
    finally:
        pg.quit()
