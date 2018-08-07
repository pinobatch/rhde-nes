#!/usr/bin/env python3

def pb8_pack_plane(plane, top_value=0x00):
    """pack a 8 byte plane into a pb8 packet.

    The pb8 packet has a 1 byte header.
    for each bit in header:
        0: Duplicate the previous byte. The first previous byte is 0x00
        1: Take a new byte from the data stream.
    """
    cplane = bytearray(1)
    flags = 0
    prev_byte = top_value
    for byte in plane[0:8]:
        flags = flags << 1
        if byte != prev_byte:
            flags = flags | 1
            cplane.append(byte)
        prev_byte = byte
    cplane[0] = flags
    return bytes(cplane)

def pb8_unpack_plane(cplane, top_value=0x00):
    cplane_iter = iter(cplane)
    plane = bytearray(8)
    flags = next(cplane_iter)
    cur_byte = top_value
    for i in range(8):
        flags = flags << 1
        if flags & 0x0100:
            cur_byte = next(cplane_iter)
        plane[i] = cur_byte
    return bytes(plane)

def menu_furni_compress(chrdata):
    out = bytearray()
    seekpoints = []
    for block in (chrdata[i:i+64] for i in range(0, len(chrdata), 64)):
        seekpoints.append(len(out))
        if len(block) < 64:
            block = block + bytes(64-len(block))
        cblock = [b'']
        plane_def = 0x00
        for plane_l, plane_m in ((block[i+0:i+8], block[i+8:i+16]) for i in range(0,64,16)):
            plane_def = plane_def << 2
            plane_l = bytes( plane_l[i] ^ plane_m[i] for i in range(8) )
            cplane_l = pb8_pack_plane(plane_l, 0xff)
            cplane_m = pb8_pack_plane(plane_m, 0x00)
            if cplane_l != b'\x00':
                cblock.append(cplane_l)
                plane_def = plane_def | 2
            if cplane_m != b'\x00':
                cblock.append(cplane_m)
                plane_def = plane_def | 1
        cblock[0] = bytes([plane_def])
        out.extend(b''.join(cblock))
    return(bytes(out), seekpoints)

def menu_furni_uncompress(data, numTiles=None):
    out = bytearray()
    it = iter(data)
    for plane_def in it:
        if numTiles is not None and len(out) >= numTiles * 64:
            break
        block = []
        for offset in range(0,64,16):
            if plane_def & 0x80:
                plane_l = pb8_unpack_plane(it, 0xff)
            else:
                plane_l = b'\xff'*8
            if plane_def & 0x40:
                plane_m = pb8_unpack_plane(it, 0x00)
            else:
                plane_m = b'\x00'*8
            plane_l = bytes( plane_l[i] ^ plane_m[i] for i in range(8) )
            block.append(plane_l)
            block.append(plane_m)
            plane_def = (plane_def << 2) & 0xff
        print(len(block), len(b''.join(block)))
        out.extend(b''.join(block))
    return bytes(out)

# copypaste argument procsessing from pb53.py
# Copyright 2012-2015 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#
def parse_argv(argv):
    from optparse import OptionParser
    parser = OptionParser(usage="usage: %prog [options] [[-i] INFILE [[-o] OUTFILE]]")
    parser.add_option("-d", "--unpack", dest="unpacking",
                      help="unpack instead of packing",
                      action="store_true", default=False)
    parser.add_option("--raw", dest="withHeader",
                      help="don't write 2-byte length and segment seek points",
                      action="store_false", default=True)
    parser.add_option("-i", "--input", dest="infilename",
                      help="read input from INFILE", metavar="INFILE")
    parser.add_option("-o", "--output", dest="outfilename",
                      help="write output to OUTFILE", metavar="OUTFILE")
    (options, args) = parser.parse_args(argv[1:])

    # Fill unfilled roles with positional arguments
    argsreader = iter(args)
    infilename = options.infilename
    if infilename is None:
        try:
            infilename = next(argsreader)
        except StopIteration:
            infilename = '-'
    if infilename == '-' and options.unpacking:
        import sys
        if sys.stdin.isatty():
            raise ValueError('cannot decompress from terminal')

    outfilename = options.outfilename
    if outfilename is None:
        try:
            outfilename = next(argsreader)
        except StopIteration:
            outfilename = '-'
    if outfilename == '-' and not options.unpacking:
        import sys
        if sys.stdout.isatty():
            raise ValueError('cannot compress to terminal')

    return (infilename, outfilename, options.unpacking, options.withHeader)

argvTestingMode = True

def main(argv=None):
    import sys
    if argv is None:
        argv = sys.argv
        if (argvTestingMode and len(argv) < 2
            and sys.stdin.isatty() and sys.stdout.isatty()):
            argv.extend(input('args:').split())
    try:
        (infilename, outfilename, unpacking, withHeader) = parse_argv(argv)
    except Exception as e:
        import sys
        sys.stderr.write("%s: %s\n" % (argv[0], str(e)))
        sys.exit(1)

    # Read input file
    infp = None
    try:
        if infilename != '-':
            infp = open(infilename, 'rb')
        else:
            infp = sys.stdin
        data = infp.read()
    finally:
        if infp and infilename != '-':
            infp.close()
        del infilename, infp

    if unpacking:
        # Decompress input file
        if withHeader:
            startOffset = data[0] * 256 + data[1]
        else:
            startOffset = 0
        outdata = menu_furni_uncompress(data[startOffset:])
    else:
        # Compress input file
        (outdata, seekpoints) = menu_furni_compress(data)
        if withHeader:
            # The file header is a list of 16-bit big endian seek points,
            # relative to the start of file.
            # header size is implied by first number.
            header_size = len(seekpoints) * 2
            header_words = list((i+header_size) % 0x10000 for i in seekpoints)
            header = b"".join(bytes([(sz >> 8) & 0xFF, sz & 0xFF])
                              for sz in header_words)
            outdata = header + outdata

    # Read input file
    outfp = None
    try:
        if outfilename != '-':
            outfp = open(outfilename, 'wb')
        else:
            outfp = sys.stdout
        outfp.write(outdata)
    finally:
        if outfp and outfilename != '-':
            outfp.close()

if __name__=='__main__':
    main()
