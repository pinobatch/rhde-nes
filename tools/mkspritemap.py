#!/usr/bin/env python3
from __future__ import with_statement, print_function, division
import sys, array
from PIL import Image, ImageDraw
from pilbmp2nes import pilbmp2chr
import pb53
from binascii import b2a_hex

try:
    xrange
except NameError:
    xrange = range

vis = False

def load_spec_file(filename):
    with open(filename, "rU") as infp:
        lines1 = (line.strip() for line in infp)
        lines2 = [line for line in lines1 if line and not line.startswith(';')]
    return lines2

def main(argv=None):
    argv = argv or sys.argv
    specfilename = argv[1]
    imname = argv[2]
    outfilename = argv[3]
    lines = load_spec_file(specfilename)
    coords = []
    for line in lines:
        coord = [x.strip() for x in line.split(',')]
        if len(coord) != 2 or not all(x.isdigit() for x in coord):
            print("%s is not coords" % line, sys.stderr)
            continue
        coords.append(tuple(int(x) for x in coord))

    im = Image.open(imname)
    if im.mode != 'P':
        print("mkspritemap.py: %s must be indexed" % imname,
              file=sys.stderr))
        sys.exit(1)

    overdraw = [0]*im.size[1]
    alltiles = []
    out = array.array('B', [len(coords)])
    for left, top in coords:
        out.extend((left, top))
        for y in xrange(top, top + 8):
            overdraw[y] += 1
        tdata = pilbmp2chr(im.crop((left, top, left+8, top+8)))
        alltiles.extend(tdata)
    compressed = pb53.pb53(''.join(alltiles))[0]
    out.fromstring(compressed)
    print("Num tiles: %d; max overdraw: %d; data size: %d bytes"
          % (len(alltiles), max(overdraw), len(compressed)))
    with open(outfilename, 'wb') as outfp:
        outfp.write(out.tostring())

    # visualize the sprites
    if vis:
        im2 = im.copy()
        dc = ImageDraw.Draw(im2)
        for left, top in coords:
            dc.rectangle((left, top, left+7, top+7), outline=1)
        for y, w in enumerate(overdraw):
            if w > 0:
                dc.rectangle((0, y, w * 7 - 1, y), fill=2)
        dispsize = tuple(s*2 for s in im.size)
        im2.resize(dispsize).show()

if __name__=='__main__':
    main()
