#!/bin/sh
ffmpeg -f rawvideo -r 5 -pix_fmt rgb24 -s 128x128 -y -an -i walk2013_out.raw -sws_flags neighbor+full_chroma_inp -vf "setsar=8/7,crop=128:112,scale=256:224" walk2013_out.webm

# sws_flags
# thanks to http://ingomar.wesp.name/2011/04/dosbox-gameplay-video-capture.html