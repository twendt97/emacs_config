;;; my-hilit.el --- HILIT (hl319.el) CUSTOMIZATIONS (obsolete)

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2003-August-31 (22:52)
;;----------------------------------------------


;;(if (file-exists-p "/usr/share/emacs/site-lisp/hl319.el")
(progn
;;  (require 'hl319);; highliting stuff
  (require 'hilit19);; highliting stuff

      ;; My color customization:
      ;; type 'alt-x list-colors-display' to see available colors.
      ;; (or look into your `rgb.txt' file)
      ;; Settings are optimized 'recognizing efficiency' without
      ;; causing *too* much eye-pain.
      ;; color1/color2 sets foreground/background
      (setq-default hilit-user-face-table
                    ;; face         light         dark         mono
                    '(
                      ;;(keyword     Firebrick          yellow   bold-italic)
                      (keyword      MediumBlue          yellow   bold-italic)
                      (type         orange             Goldenrod     nil)
;;                      (comment      grey80             DeepPink      italic) ;; disappear1
;;                      (comment      white/Gray92       DeepPink      italic) ;; disappear2
;;                      (comment      brown/Gray92       DeepPink      italic) ;; with blocks
                      (comment      brown              DeepPink      italic)
                      (string       forestgreen        moccasin      underline)
                      (operator     black-bold         yellow-bold   underline)
                      (include      magenta            Plum1         bold-italic)
;;                      (define       DarkTurquoise-underline green-bold    bold)
;;                      (define       magenta-underline green-bold    bold)
                      (define       blueviolet        green-bold    bold)
                      (defun        blue-bold          cyan-bold     bold-italic)
                      (decl         RoyalBlue-underline     yellow        bold)
                      (label        red-bold           red-bold underline)
                      (formula      forestgreen        green         underline)
                      (rule    VioletRed-underline  cyan-bold-underline default-bold-underline)
                      ))

;;            (if (file-exists-p "/usr/share/emacs/site-lisp/hilit-LaTeX.el")
;;                (require 'hilit-LaTeX))

      ;; set hilit-local-ctypes to a list of regular expressions
      ;; that match the typedefs that
      ;; you commonly use in your C/C++ sourcefiles.
      ;; Ex: '("\\w+_[Tt]") is one pattern that's quite useful... but it's slow.
      (setq hilit-local-ctypes '(
                                 "size_t"
                                 "Complex"
                                 "ldouble";; long double
                                 "uint" "ulong";; unsigned
                                 "int32" "uint32" "int64" "uint64"
                                 ;; GNU types:
                                 "Integer" "Rational"
                                 ;; for mod:
                                 "mod" "umod_t" "smod_t"
                                 ;; for hfloat:
                                 "hfloat" "hfguts" "hfdata" "LIMB"
                                 ;; for templates:
                                 "Type" "Type0" "Type1" "Type2" "Type3" "Type4"
                                 ))

      ;; auto-highlight is disabled in buffers larger than this:
      (setq hilit-auto-highlight-maxout 300000)

      ;; If non-nil, this inhibits progress indicators during highlighting:
      (setq hilit-quietly nil)

      ;; If this is non-nil, then hilit-redraw and hilit-recenter will also
      ;; rehighlight part or all of the current buffer.  T will rehighlight the
      ;; whole buffer, a NUMBER will rehighlight that many lines before and after
      ;; the cursor, and the symbol 'visible' will rehighlight only the visible
      ;; portion of the current buffer.  This variable is buffer-local.
      (setq-default hilit-auto-rehighlight 200)

;;      (global-set-key "\C-l" 'hilit-recenter)
      (global-set-key [?\C-\S-l] 'hilit-repaint-command)

      ;; `t' only useful on changing fonts
      (setq hilit-face-check nil)

      )
;;)


;;; file ends here

