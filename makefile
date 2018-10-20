#!/usr/bin/make -f
#
# Makefile for NES game
# Copyright 2011-2018 Damian Yerrick
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
version = 0.07-wip

# Assembly language files that make up the PRG ROM
align_sensitive_modules := vwf7 paldetect
game_modules := \
  main flood bg alert connect random title racesel \
  makedoors shopping furnistock hra \
  battle missiles droppedfurni build
lib_modules := vwf_draw ppuclear pads bcd unpb53 donut math
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
DOTEXE:=.exe
EMU := start ""
DEBUGEMU := start ""
PY := py -3
else
DOTEXE:=
EMU := fceux
DEBUGEMU :=  ~/.wine/drive_c/Program\ Files\ \(x86\)/FCEUX/fceux.exe
PY := python3
endif

.PHONY: run dist zip clean all debug

run: $(title).nes
	$(EMU) $<
debug: $(title).nes
	$(DEBUGEMU) $<

all: $(title).nes

clean:
	-rm $(objdir)/*.o $(objdir)/*.chr $(objdir)/*.s $(objdir)/*.sav
	-rm $(objdir)/*.pb53 $(objdir)/*.donut $(objdir)/*.minid $(objdir)/*.ov53

# Rule to create or update the distribution zipfile by adding all
# files listed in zip.in.  Actually the zipfile depends on every
# single file in zip.in, but currently we use changes to the compiled
# program, makefile, and README as a heuristic for when something was
# changed.  It won't see changes to docs or tools, but usually when
# docs changes, README also changes, and when tools changes, the
# makefile changes.
dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in $(title).nes README.md USAGE.html CHANGES.txt $(objdir)/index.txt
	$(PY) tools/zipup.py $< $(title)-$(version) -o $@

# Build zip.in from the list of files in the Git tree
zip.in:
	git ls-files | grep -e "^[^.]" > $@
	echo $(title).nes >> $@
	echo zip.in >> $@

$(objdir)/index.txt: makefile
	echo Files produced by build tools go here, but caulk goes where? > $@

# Rules for PRG ROM

objlistntsc = $(foreach o,$(objlist),$(objdir)/$(o).o)

map.txt $(title).nes: nrom256.cfg $(objlistntsc)
	$(LD65) -o $(title).nes -C $^ -m map.txt

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.inc $(srcdir)/global.inc $(srcdir)/mbyt.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

# Files that depend on .incbin'd files
$(objdir)/main.o: \
  $(objdir)/bggfx.chr.donut $(objdir)/spritegfx.chr.donut \
  $(objdir)/nander_sprites.chr.donut $(objdir)/poli_sprites.chr.donut \
  $(objdir)/volci_sprites.chr.donut $(objdir)/furni_sprites.chr.donut
$(objdir)/title.o: \
  TODO.txt $(objdir)/titleguy.chr.donut $(srcdir)/uctions1.txt\
  $(srcdir)/uctions2.txt $(srcdir)/uctions3.txt $(srcdir)/uctions4.txt \
  $(objdir)/title1.nam.donut $(objdir)/title1.chr.donut \
  $(objdir)/title_cursor_sprites.chr.donut $(srcdir)/uctions5.txt
$(objdir)/racesel.o: \
  $(objdir)/rsel.nam.donut $(objdir)/rsel.chr.donut \
  $(objdir)/racesel_sprites.chr.donut
$(objdir)/shopping.o: $(objdir)/menu_furni.donut

# Generate lookup tables

$(objdir)/ntscPeriods.s: tools/mktables.py
	$(PY) $< period $@

# Rules for graphics data conversion

$(objdir)/menu_furni.pb53: $(objdir)/menu_furni16.chr
	$(PY) tools/pb53.py --block-size=4 --no-prev $< $@

$(objdir)/menu_furni.donut: $(objdir)/menu_furni16.chr
	$(PY) tools/donut.py -f --no-bit-flip --no-prev --add-block-seekpoints $< $@

$(objdir)/%.donut: $(objdir)/%
	$(PY) tools/donut.py -f $< $@

$(objdir)/%.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py $< $@

$(objdir)/%16.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py -H 16 $< $@

$(objdir)/rsel.sav: $(imgdir)/racesel_screen.png
	$(PY) tools/savtool.py --palette=0f1637060f381A320f0f0f0f0f0f0f0f $< $@

$(objdir)/title1.sav: $(imgdir)/rhdescene_thiefonly_locolor.png
	$(PY) tools/savtool.py --palette=0f1016290f1011290f1027290f101722 $< $@

$(objdir)/%.nam: $(objdir)/%.sav
	$(PY) tools/savtool.py $< $@

$(objdir)/%.chr: $(objdir)/%.sav
	$(PY) tools/savtool.py $< $@

$(objdir)/%.pb53: $(objdir)/%
	$(PY) tools/pb53.py --raw $< $@

$(objdir)/%.s: tools/vwfbuild.py tilesets/%.png
	$(PY) $^ $@

# Currently unused

$(objdir)/title2.sav: $(imgdir)/titlewordmark.png
	$(PY) tools/savtool.py --palette=0f1016290f1012290f0016290f161229 $< $@

$(objdir)/titlescreensprites.ov53: tools/mkspritemap.py \
$(srcdir)/titlescreensprites.in $(imgdir)/titlespritelayer.png
	$(PY) $^ $@

