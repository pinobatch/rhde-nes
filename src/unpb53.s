;
; PB53 unpacker for 6502 systems
; Copyright 2012 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.export PB53_outbuf
.exportzp ciSrc, ciDst, ciBufStart, ciBufEnd
;.export unpb53_some

.segment "ZEROPAGE"
ciSrc: .res 2
ciDst: .res 2
ciBufStart: .res 1
ciBufEnd: .res 1
PB53_outbuf = $0100
