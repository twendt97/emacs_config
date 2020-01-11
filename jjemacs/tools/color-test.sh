

# to check the colors of font-lock:

H=/home/jj

F=""
F="$F $H/fxt/makefile"
F="$F $H/fxt/fht/fht0.cc"
F="$F $H/hfloat/src/include/hfloat.h"
F="$F $H/.profile"
F="$F /usr/local/bin/replace"
F="$F $H/work/busi/webshop/makelist.pl"
F="$F $H/www/public_html/joerg.html"
F="$F $H/.jjemacs/xwin.el"
F="$F $H/.jjemacs/funcs.el"
F="$F $H/work/mupad/algdep.mu"

#echo "F=[$F]";

x="";
for f in $F; do
#    echo "$f:"
    if [ -f $f ]; then
        x="$f $x";
#        echo "x=[$x]";
    else
        echo "$f  not found";
    fi
done

#echo "x=[$x]"

emacs $x &

exit 0;
#################################
