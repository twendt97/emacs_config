;;; main.el --- main file

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-April-01 (14:19)
;;----------------------------------------------

;; (to be loaded from the ~/.emacs file)
;; i.e. your ~/.emacs file should contain the line
;; (load "~/.jjemacs/main.el")

;; also see the System wide start file for emacs:
;; /usr/share/emacs/site-lisp/site-start.el


;; append current dir to load-path
;; edit if your directory for these files is different:
(setq load-path (append load-path '("~/.emacs.d/jjemacs/")))
(setq load-path (append load-path '("~/.emacs.d/jjemacs/other/")))
;;
;; following needs massaging (intrusive piece of crap):
;;(setq load-path (append load-path '("/usr/local/share/emacs/site-lisp/pari/")))
;; massaged version is here:
(setq load-path (append load-path '("~/.emacs.d/jjemacs/other/pari/")))


;; used with mode-line and frame title:
(setq hostname (getenv "HOSTNAME"))


;; line numbers at start of each line:
(load "linum.el")
;; activate with  M-x linum-mode
;; or  M-x global-linum-mode

;; highlight the current line:
(load "highline.el")
;;(require 'highline.el)
;; use  M-x highline-mode  (also in jj-funcs menu)
;; or  M-x global-highline-mode

;; latex:
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;; pari/gp's files are under other/pari:
;;(autoload 'gp-script-mode "/usr/local/lib/pari/emacs/pari" "pari gp" nil  t)
(autoload 'gp-script-mode "pari" "pari gp" nil  t) ;; just this one!
;;(autoload 'gp "pari" "pari gp" nil t)
;;(autoload 'gp-mode "pari" "pari gp" t)
;;(autoload 'gpman "pari" "pari-gp manual" nil  t)
;;(autoload 'gp "pari" "pari gp" t)
;;(autoload 'gpman "pari" " pari-gp manual" t)

(autoload 'gap-mode "gap-mode" "Gap editing mode" t)

(setq load-path (append load-path '("~/.jjemacs/other/maxima/")))
(autoload 'maxima-mode "maxima/maxima.el" "Maxima mode" t)

;;(autoload 'mupad-mode "mupad-mode" "mupad Mode" t)

(autoload 'maplev-mode "maplev" "maple Mode" t)
(autoload 'mma-mode "mma.el" "Mathematica package file mode" t)

(autoload 'ruby-mode "ruby-mode" "ruby Mode" t)

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;;(autoload 'html-helper-mode "html-helper-mode" "HTML Helper Mode" t)


;;(require 'htmlize)
(autoload 'htmlize-buffer "htmlize" "htmlize" t)
(autoload 'htmlize-file "htmlize" "htmlize" t)
(autoload 'htmlize-region "htmlize" "htmlize" t)

;;(autoload 'python-mode "python-mode" "python Mode" t)

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)

;; doesn't seem to work:
;;(autoload 'highlight-parentheses-mode
;;  "highlight-parentheses-mode" "Highlight-Parentheses Editing Mode" t)



;; Load own files only after loading packages:
(load "modes.el")
(load "hooks.el")
(load "comments.el")
(load "funcs.el")
(load "funcs-tex.el")
(load "keys.el")
(load "fonts.el")
(load "menu.el")
(load "mode-line.el")
(load "xwin.el")
(load "my-font-lock.el")
;;(load "dim-colors.el") ; loaded by my-font-lock.el
(load "settings.el")
(load "jjproject.el")
(load "playground.el")

;; If you copy my config, but want to override
;; some things, keep a file ~/.emacs-final.el
(if (file-exists-p "~/.emacs-final.el")
     (load "~/.emacs-final.el") )


;;(provide 'main)

;;; main.el ends here
