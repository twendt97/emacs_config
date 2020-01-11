#! /usr/bin/env bash

# version: 2005-July-24

# usage:
# export PSFILE=whatever.ps  ##  defaults to $CTMPDIR/tmp.ps
## export PNGBASE=whatever  ##  defaults to $CTMPDIR/tmp%02d.png
# export DATE=whatever  ## defaults to $(date -r filename)
# export MODE=whatever  ## defaults to mode according to file
# export NOCOLOR=yes  ## (set if B/W only is wanted)
#  $0 filename dname

test -n "$1" || exit 1;
test -f $1 || exit 2;
export FILE=$1

CTMPDIR=/dev/shm/cps/  ## magic default
mkdir -p $CTMPDIR  ||  exit 33
test -w  $CTMPDIR  ||  exit 33
test -n "$PSFILE" || PSFILE=/$CTMPDIR/tmp.ps
export PSFILE
rm -f $PSFILE

DNAME=$FILE
test -n "$2" && DNAME=$2
export DNAME

test -n "$NOCOLOR" || NOCOLOR=""
export NOCOLOR

test -n "$DATE" || DATE=$(date +'%Y-%B-%d' -r $FILE)
#test -n "$DATE" || DATE="$(date +'%Y-%B-%d  %H:%M:%S' -r $FILE)"
export DATE

emacs -iconic -name 'color-ps-file' -geometry +50+0 -d :0.0 -l ~/.jjemacs/tools/color-ps-file.el \
  || { echo "emacs barfed (exiting)."; exit 4; }
test -f $PSFILE  ||  exit 1
ls -lF  $PSFILE
gv $PSFILE &

exit 0;
########################

## compress ps:
ZPSFILE=$CTMPDIR/$(basename $PSFILE).gz
gzip -9 $PSFILE -c > $ZPSFILE
gv $ZPSFILE &

ls -lF $ZPSFILE


exit 0;
########################

## create pixel-graphics:


cd  $CTMPDIR  ||  exit 33

rm -f *.ppm
rm -f *.png

GSFLAGS=-r150x150

## bad quality:
#GFILES=tmp%02d.png
#gs -sDEVICE=png16m $GSFLAGS -dNOPAUSE -dBATCH -sOutputFile=$GFILES $PSFILE


## therefore use ppm as intermediate:
GFILES=tmp%02d.ppm
gs -sDEVICE=ppmraw $GSFLAGS -dNOPAUSE -dBATCH -sOutputFile=$GFILES $PSFILE

XGFILES=tmp[0-9]*.ppm
#ls -l $XGFILES
#xli -geometry +300+20 $XGFILES
for f in $XGFILES; do
    PNGFILE=$(basename $f .ppm).png;
    pnmtopng -compression 9 $f > $PNGFILE
done
rm *.ppm

ls -ltr --color=tty
file *.png
echo -n "size of *.png:  "
du -sch *.png | tail -n 1
pwd

exit 0;
########################
