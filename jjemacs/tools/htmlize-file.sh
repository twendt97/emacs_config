#! /usr/bin/env bash

# usage: htmlize-file.sh FILE > OUT

test -n "$1" || exit 1;
test -f $1 || exit 2;
export FILE=$1


emacs -iconic -name 'htmlize-file' -geometry +50+0 -d :0.0 -l ~/.jjemacs/tools/htmlize-file.el $1 \
  || { echo "emacs barfed (exiting)."; exit 4; }

cat tmp-htmlize-out-tmp.html
rm tmp-htmlize-out-tmp.html

exit 0;
################################
