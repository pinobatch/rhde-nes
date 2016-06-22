#!/usr/bin/make -f
#
# Makefile for NES game
# Copyright 2011 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the NES program and the zip file.
title = rhde

#CURDAY := $(shell echo $$(( ($$(date -d 'now' '+%s') / 86400) - 15928 )))
#version = day$(CURDAY)
version = forgithub

# Assembly language files that make up the PRG ROM
align_sensitive_modules := vwf7 paldetect
game_modules := \
  main flood bg alert connect random title racesel \
  makedoors shopping furnistock hra \
  battle missiles droppedfurni build
lib_modules := vwf_draw ppuclear pads bcd unpb53 math
audio_modules := sound music musicseq ntscPeriods
objlist := $(align_sensitive_modules) $(game_modules) \
  $(lib_modules) $(audio_modules)

AS65 = ca65
LD65 = ld65
CFLAGS65 = -DUSE_DAS=1
objdir = obj/nes
srcdir = src
imgdir = tilesets

#EMU := "/C/Program Files/Nintendulator/Nintendulator.exe"
EMU := fceux
# other options for EMU are start (Windows) or gnome-open (GNOME)

# Occasionally, you need to make "build tools", or programs that run
# on a PC that convert, compress, or otherwise translate PC data
# files into the format that the NES program expects.  Some people
# write their build tools in C or C++; others prefer to write them in
# Perl, PHP, or Python.  This program doesn't use any C build tools,
# but if yours does, it might include definitions of variables that
# Make uses to call a C compiler.
CC = gcc
CFLAGS = -std=gnu99 -Wall -DNDEBUG -O

# Windows needs .exe suffixed to the names of executables; UNIX does
# not.  COMSPEC will be set to the name of the shell on Windows and
# not defined on UNIX.
ifdef COMSPEC
DOTEXE=.exe
else
DOTEXE=
endif

.PHONY: run dist zip clean

run: $(title).nes
	$(EMU) $<

clean:
	-rm $(objdir)/*.o $(objdir)/*.chr $(objdir)/*.ov53 $(objdir)/*.sav $(objdir)/*.pb53 $(objdir)/*.s

# Rule to create or update the distribution zipfile by adding all
# files listed in zip.in.  Actually the zipfile depends on every
# single file in zip.in, but currently we use changes to the compiled
# program, makefile, and README as a heuristic for when something was
# changed.  It won't see changes to docs or tools, but usually when
# docs changes, README also changes, and when tools changes, the
# makefile changes.
dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in $(title).nes README.html CHANGES.txt $(objdir)/index.txt
	zip -9 -u $@ -@ < $<

$(objdir)/index.txt: makefile
	echo Files produced by build tools go here, but caulk goes where? > $@

# Rules for PRG ROM

objlistntsc = $(foreach o,$(objlist),$(objdir)/$(o).o)

map.txt $(title).nes: nes.ini $(objlistntsc)
	$(LD65) -o $(title).nes -C $^ -m map.txt

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.h $(srcdir)/ram.h $(srcdir)/mbyt.h
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

# Files that depend on .incbin'd files
$(objdir)/main.o: \
  $(objdir)/bggfx.chr.pb53 $(objdir)/spritegfx.chr.pb53 \
  $(objdir)/nander_sprites.chr.pb53 $(objdir)/poli_sprites.chr.pb53 \
  $(objdir)/volci_sprites.chr.pb53 $(objdir)/furni_sprites.chr.pb53
$(objdir)/title.o: \
  TODO.txt $(objdir)/titleguy.chr.pb53 $(srcdir)/uctions1.txt\
  $(srcdir)/uctions2.txt $(srcdir)/uctions3.txt $(srcdir)/uctions4.txt \
  $(objdir)/title1.nam.pb53 $(objdir)/title1.chr.pb53 \
  $(objdir)/title_cursor_sprites.chr.pb53 $(srcdir)/uctions5.txt
$(objdir)/racesel.o: \
  $(objdir)/rsel.nam.pb53 $(objdir)/rsel.chr.pb53 \
  $(objdir)/racesel_sprites.chr.pb53
$(objdir)/shopping.o: $(objdir)/menu_furni.pb53

# Generate lookup tables

$(objdir)/ntscPeriods.s: tools/mktables.py
	$< period $@

# Rules for graphics data conversion

$(objdir)/menu_furni.pb53: $(objdir)/menu_furni16.chr
	tools/pb53.py --block-size=4 --no-prev $< $@

$(objdir)/%.pb53: $(objdir)/%
	tools/pb53.py --raw $< $@

$(objdir)/%.chr: $(imgdir)/%.png
	tools/pilbmp2nes.py $< $@

$(objdir)/%16.chr: $(imgdir)/%.png
	tools/pilbmp2nes.py -H 16 $< $@

$(objdir)/rsel.sav: $(imgdir)/racesel_screen.png
	tools/savtool.py --palette=0f1637060f381A320f0f0f0f0f0f0f0f $< $@

$(objdir)/title1.sav: $(imgdir)/rhdescene_thiefonly_locolor.png
	tools/savtool.py --palette=0f1016290f1011290f1027290f101722 $< $@

$(objdir)/%.nam: $(objdir)/%.sav
	tools/savtool.py $< $@

$(objdir)/%.chr: $(objdir)/%.sav
	tools/savtool.py $< $@

$(objdir)/%.pb53: $(objdir)/%
	tools/pb53.py --raw $< $@

$(objdir)/%.s: tools/vwfbuild.py tilesets/%.png
	$^ $@

# Currently unused

$(objdir)/title2.sav: $(imgdir)/titlewordmark.png
	tools/savtool.py --palette=0f1016290f1012290f0016290f161229 $< $@

$(objdir)/titlescreensprites.ov53: tools/mkspritemap.py \
$(srcdir)/titlescreensprites.in $(imgdir)/titlespritelayer.png
	$^ $@

