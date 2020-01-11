;;; xwin.el --- stuff specific to X11

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-February-23 (09:16)
;;----------------------------------------------


;;; STUFF FOR NON X11:
(if (not (eq window-system 'x))
  (progn

    ;; paren matching on:
    (require 'paren)

    (global-highline-mode -1) ;; nasty with dark background
    ) )


;;; STUFF FOR X11:
(if (eq window-system 'x)
    (progn

      ;;; https://www.jwz.org/doc/x-cut-and-paste.html
      ;;; http://www.emacswiki.org/emacs/CopyAndPaste
      ;; "Support for X cut buffers has been removed." (ver >=24.1)
      ;;
      ;; TO-DO with Emacs 25.*:
      ;; 'x-select-text' is renamed 'gui-select-text'.
      ;; 'x-selection-value' is renamed 'gui-selection-value'.
      ;; 'x-get-selection' is renamed 'gui-get-selection'.
      ;; 'x-get-clipboard' and 'x-clipboard-yank' are marked obsolete.
      ;; 'x-get-selection-value' is renamed to 'gui-get-primary-selection'.
      ;; 'x-set-selection' is renamed to 'gui-set-selection'.
      ;;
      ;; If non-nil, mouse drag copies region to kill-ring:
      (setq mouse-drag-copy-region t) ;; defaults to nil now (ver >=24.1)
      ;;  make a killed region available to other programs:
      (setq interprogram-cut-function 'x-select-text)
      ;; get text cut from other programs:
      (setq interprogram-paste-function 'x-selection-value)
      ;; Non-nil means cutting and pasting uses the primary selection:
      (setq x-select-enable-primary t)
      (setq x-select-enable-clipboard t)
;;      (global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
      (delete-selection-mode)
      ;;
      (global-set-key [(shift insert)] 'jjj-insert-primary)  ;; for versions >= 24
      ;;; cf.:
      ;; <mouse-2> runs the command mouse-yank-primary

      (global-highline-mode 1) ;; OK with X11
      ;; can be toggled for current buffer from jj-funcs menu

      ;; hide toolbar with emacs version >=21:
      ;; (see settings.el )
;;      (if (>= emacs-major-version 21) (tool-bar-mode -1))

      ;; Specify whether to have vertical scroll bars, and on which side.
      ;; Possible values are nil (no scroll bars), `left' and `right':
      (set-scroll-bar-mode 'left)

;;      (require 'paren)
;;      (require 'mic-paren)
      (require 'stig-paren)

;;      (setq blink-matching-paren t)
      (setq paren-dingaling-mode t)
;;      ;; Non-nil causes paren-command-hook to highlight whole S-expressions:
      (setq paren-sexp-mode nil) ;; colorize space between parens
;;      (global-set-key [(control \()] 'stig-paren-toggle-dingaling-mode)
;;      (global-set-key [(control \))] 'stig-paren-toggle-sexp-mode)

      ;;      (setq paren-mismatch-face 'underline)
      (make-face 'jjj-paren-mismatch-face)
      (set-face-background 'jjj-paren-mismatch-face "DeepPink")
      ;;      (set-face-underline-p 'jjj-paren-mismatch-face t)
      ;;      (set-face-stipple 'jjj-paren-mismatch-face '(8 8 "A5A5A5A5A5"))
      (setq paren-mismatch-face 'jjj-paren-mismatch-face)
;;      (setq show-paren-mismatch-face 'jjj-paren-mismatch-face)
      ;;
      (make-face 'jjj-paren-match-face)
      (set-face-background 'jjj-paren-match-face "cyan")
      (setq paren-match-face 'jjj-paren-match-face)
      ;;


;;;;      ;; hilit used only for html-helper-mode (obsolete by now):
;;      (setq hilit-mode-enable-list  '(html-helper-mode))
;;      (load "my-hilit.el")  ;; customize hl319.el
;;;;      ;; /usr/share/emacs/site-lisp/hl319.el


      ;; --------------- set fonts: ---------------
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
      ;; https://www.emacswiki.org/emacs/SetFonts

      ;; -------- default font:  key = kp-enter
      ;; Note: repeat entry in (setq initial-frame-alist ...) below
;;      (defconst jj-normal-font "7x13")
;;      (defconst jj-normal-font "8x13bold")
;;      (defconst jj-normal-font "10x20")
;;      (defconst jj-normal-font "DejaVu Sans Mono-10:bold")
      (defconst jj-normal-font "DejaVu Sans Mono-14:bold")
      ;; 8x13bold     -misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1
      ;;
      (global-set-key [(control kp-enter)] '(lambda () (interactive) (set-default-font jj-normal-font)))
      (define-key esc-map [(kp-enter)] '(lambda () (interactive) (set-default-font jj-normal-font)))
      ;; Needs to be copied to initial-frame-alist below,
      ;; using jj-normal-font does not work there.

      ;; -------- larger font:  key = kp-add

;;      (defconst jj-large-font "9x15bold")
      ;;      (defconst jj-large-font "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1") ;; emap
      ;; /usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf
;;      (defconst jj-large-font "DejaVu Sans Mono-12") ;; emap
;;      (defconst jj-large-font "DejaVu Sans Mono-10")
;;      (defconst jj-large-font "DejaVu Sans Mono-10:bold")
      (defconst jj-large-font "DejaVu Sans Mono-16:bold")
      ;;
      (global-set-key [(control kp-add)] '(lambda () (interactive) (set-default-font jj-large-font)))
      (define-key esc-map [(kp-add)] '(lambda () (interactive) (set-default-font jj-large-font)))
;;      (define-key esc-map [(kp-add)] '(lambda () (interactive)
;;                                        (progn
;;                                              (set-face-attribute 'default nil :font jj-large-font )
;;                                              (set-frame-font jj-large-font nil t)
;;                                          ) ) )

      ;; -------- alt font:  key = kp-subtract
      ;; ls -l /usr/share/fonts/X11/misc/
      (defconst jj-utf8-font "10x20")
;;      (defconst jj-utf8-font "9x15bold")
      ;; 9x15bold == -misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1
;;      (defconst jj-utf8-font "-b&h-lucidatypewriter-medium-r-normal-sans-12-120-75-75-m-70-iso10646-1")
;;      (defconst jj-utf8-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO10646-1")
      ;;
      (global-set-key [(control kp-subtract)] '(lambda () (interactive) (set-default-font jj-utf8-font)))
      (define-key esc-map [(kp-subtract)] '(lambda () (interactive) (set-default-font jj-utf8-font)))

      ;; -------- smaller fonts:  key = kp-divide

;;      (defconst jj-small-font "6x10")
      (defconst jj-small-font "DejaVu Sans Mono-8:bold")  ;; has zeros 0 != O
      ;; 6x10 == -misc-fixed-medium-r-normal--10-100-75-75-c-60-iso8859-1
      ;;
      (global-set-key [(control kp-divide)] '(lambda () (interactive) (set-default-font jj-small-font)))
      (define-key esc-map [(kp-divide)] '(lambda () (interactive) (set-default-font jj-small-font)))

      ;; -------- smaller fonts:  key = kp-delete

;;      (defconst jj-tiny-font "5x8")  ;; super tiny
;;      (defconst jj-tiny-font "DejaVu Sans Mono-8:bold")  ;; small but readable
      (defconst jj-tiny-font "8x13bold")  ;; former default font
      ;;
      (global-set-key [(control kp-delete)] '(lambda () (interactive) (set-default-font jj-tiny-font)))
      (define-key esc-map [(kp-delete)] '(lambda () (interactive) (set-default-font jj-tiny-font)))


      ;; -------- larger font:  key = kp-multiply

;;      (defconst jj-big-font "-adobe-courier-*-r-normal-*-18-*-*-*-m-*-iso8859-1")
;;      (defconst jj-huge-font "-adobe-courier-*-r-normal--*-240-*-*-m-*-iso8859-1")
;;      (defconst jj-huge-font "lucidasanstypewriter-bold-14")
;;      (defconst jj-huge-font "DejaVu Sans Mono-10:bold")
;;      (defconst jj-huge-font "DejaVu Sans Mono-12")
      (defconst jj-huge-font "DejaVu Sans Mono-18:bold")
      ;; 10x20 == -misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1
      ;;
      (global-set-key [(control kp-multiply)] '(lambda () (interactive) (set-default-font jj-huge-font)))
      (define-key esc-map [(kp-multiply)] '(lambda () (interactive) (set-default-font jj-huge-font)))

      ;; -------- test font:  key = kp-insert
;;      (defconst jj-test-font "screen-bold18")
;;      (defconst jj-test-font "-sgi-screen-bold-r-normal--13-130-72-72-m-80-iso8859-1") ;; has zeros != O; sadly gone
;;      (defconst jj-test-font "sgi-screen-bold-r-normal--14-*-*-*-*-*-*-*") ;; has zeros != O; sadly gone
;;      (defconst jj-test-font "-*-fixed-*-*-*-*-20-*-75-*-*-*-*-*")
      (defconst jj-test-font "DejaVu Sans Mono-16:bold") ;; huge
      ;;
      (global-set-key [(control kp-insert)] '(lambda () (interactive) (set-default-font jj-test-font)))
      (define-key esc-map [(kp-insert)] '(lambda () (interactive) (set-default-font jj-test-font)))


      ;; Now set a font:
      (set-default-font jj-normal-font)

      ;; no effect on 24.3, otherwise message about missing characters:
;;      (set-face-font 'menu jj-normal-font)
      ;; ------- end (set font) -------


      ;; self-explanatory:
      (set-mouse-color "red")
      (set-background-color "white")
      (set-foreground-color "black")
      ;;      (set-border-color "green")
      (set-cursor-color "red") ;; use "darkslateblue" if "red" hurts your eyes


      (setq initial-frame-alist
            '(
              (width . 115) (height . 70) ;; cf. ~/.Xresources
              (menu-bar-lines . 1)
              (background-color . "white")
              ;;        (name . " %b ")  ;; leave unset, cf. frame-title-format
              (mouse-color . "red")
              (cursor-color . "red")
              (foreground-color . "black")
              ;;        (border-color "green")
              ;;
              ;; using jj-normal-font does not work here, so copy value:
;;              (font . "lucidasanstypewriter-bold-14")  ;; huge
              (font . "DejaVu Sans Mono-14:bold")  ;;
;;              (font . "DejaVu Sans Mono-10:bold")  ;;
;;              (font . "8x13bold")  ;;
;;              (font . "DejaVu Sans Mono-8")  ;;
              ) )
      ;; make this the default:
      (setq default-frame-alist initial-frame-alist)

      ;; frame title:
      ;; This variable has the same structure as `mode-line-format'
      ;; 1) you@host.domain.org:
;;      (setq frame-title-format '("%b  (" user-login-name "@" system-name ")"))
      ;; 2) you@host:
;;      (setq frame-title-format '("%b  (" user-login-name "@" hostname ")"))
      (setq frame-title-format
            '("E: %b  (" user-login-name "@" hostname ")    [" emacs-version "]"))
      ;; cf. variable mode-line-format

      ;; same title for icon:
;;      (setq icon-title-format frame-title-format)
      ;; shortened title for icon:
      (setq icon-title-format '("E: %b"))

      ;;      (modify-frame-parameters FRAME '((name . "voodoo")))
      ;;      (setq after-make-frame-hook 'my-after-make-frame-hook)

      ;;      (require 'xfonts)

      ;; choose color with M-x list-colors-display
      (set-face-background 'region "wheat") ; background for copy and paste
;;      (set-face-background 'menu "gainsboro") ; background for menu
      (set-face-background 'menu "LemonChiffon") ; background for menu
      (set-face-background 'scroll-bar "LemonChiffon") ; background for scroll-bar
      (set-face-background 'mode-line "LemonChiffon") ; background for mode-line

      ) )


;;(provide 'xwin)

;;; xwin.el ends here
