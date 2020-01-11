;;; my-font-lock.el --- font-lock customizations

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:09)
;;----------------------------------------------


;;(if (file-exists-p font-lock-mode-file)
;;    (setq font-lock-mode-file
;;          (concat
;;           "/usr/share/emacs/"
;;           emacs-major-version "." emacs-minor-version
;;           "/lisp/font-lock.el")))
;;(message font-lock-mode-file)



(progn
  (require 'font-lock)


  (setq-default show-trailing-whitespace t)
  ;; Non-nil means highlight trailing whitespace in face `trailing-whitespace'.
  (set-face-background 'trailing-whitespace "yellow"); cursor is red, do make it yellow
  ;;(mapc (lambda (hook)
  ;;        (add-hook hook (lambda () (setq show-trailing-whitespace t))))
  ;;      '(text-mode-hook
  ;;        c-mode-hook
  ;;        emacs-lisp-mode-hook
  ;;        shell-script-mode-hook))

  ;; always use font-lock-mode:
  (global-font-lock-mode t) ;; causes slowdown (only bad on slow machines)
  ;;(toggle-global-lazy-font-lock-mode)

  ;; Maximum size of a buffer for buffer fontification.
  ;; Only buffers less than this can be fontified when Font Lock mode is turned on.
  ;; If nil, means size is irrelevant.
  (setq font-lock-maximum-size 200000)

  (setq font-lock-maximum-decoration t)

;; Available font-lock faces:
;; font-lock-comment-face  "for comments."
;; font-lock-comment-delimiter-face   "for comment delimiters."
;; font-lock-string-face   "for strings."
;; font-lock-doc-face   "for documentation."
;; font-lock-keyword-face   "for keywords."
;; font-lock-builtin-face   "for builtins."
;; font-lock-function-name-face  "for function names."
;; font-lock-variable-name-face   "for variable names."
;; font-lock-type-face  "for type and class names."
;; font-lock-constant-face  "for constant and label names."
;; font-lock-warning-face  "for things that should stand out."
;; font-lock-preprocessor-face "for preprocessor directives."
;; font-lock-negation-char-face	 "for easy to overlook negation. This can be an \"!\" or the \"n\" in \"ifndef\"."
;;
;;  (make-face 'font-latex-math-face)   ;
;;  (make-face 'font-lock-comment-face)   ;
;;  (make-face 'font-lock-string-face)    ;
;;  (make-face 'font-lock-keyword-face)   ;
;;  (make-face 'font-lock-builtin-face)   ;
;;  (make-face 'font-lock-function-name-face) ;
;;  (make-face 'font-lock-variable-name-face) ;
;;  (make-face 'font-lock-type-face)      ; type and class names
;;  (make-face 'font-lock-constant-face)  ; constant and label names
;;  (make-face 'font-lock-warning-face) ; for things that should stand out
;;  (make-face 'font-lock-preprocessor-face) ; #include, #ifdef, etc.

;;  (setq jjj-flcff1 "brown") ;; default (at startup)
;;  (setq jjj-flcff1 "grey30") ;; default (at startup)
  (setq jjj-flcff1 "#7d4d4d") ;; default (at startup)
  (setq jjj-flcff2 "grey92") ;; almost invisible
  (set-face-foreground 'font-lock-comment-face jjj-flcff1)

;; try M-x list-colors-display to find good colors
  (make-face 'sh-heredoc)   ; for cat <<EOF in shell scripts
  (set-face-foreground 'sh-heredoc "slategray")
;;  (set-face-foreground 'font-latex-math-face "forestgreen");; ?? no effect
;;  (set-face-foreground 'font-lock-string-face "forestgreen")
  (set-face-foreground 'font-lock-string-face "darkgreen")
;;  (set-face-foreground 'font-lock-keyword-face "MediumBlue")
;;  (set-face-foreground 'font-lock-keyword-face "NavyBlue")
  (set-face-foreground 'font-lock-keyword-face "Blue3");; cf. Blue4
  (set-face-foreground 'font-lock-builtin-face "magenta")
;;  (set-face-foreground 'font-lock-preprocessor-face "DodgerBlue")
;;  (set-face-foreground 'font-lock-preprocessor-face "VioletRed")
;;  (set-face-foreground 'font-lock-preprocessor-face "purple")
;;  (set-face-foreground 'font-lock-preprocessor-face "IndianRed1")
;;  (set-face-foreground 'font-lock-preprocessor-face "firebrick2")
  (set-face-foreground 'font-lock-preprocessor-face "DeepPink2")

  (if (eq window-system 'x)
      (progn
        (set-face-foreground 'font-lock-function-name-face "darkgreen")
        (set-face-background 'font-lock-function-name-face "gray90") )
    (progn
      (set-face-foreground 'font-lock-function-name-face "darkgreen")
      (set-face-background 'font-lock-function-name-face "#080808") )
    )

  (set-face-underline-p 'font-lock-function-name-face t)
  ;;      (set-face-foreground 'font-lock-function-name-face "black")
  ;;      (set-face-background 'font-lock-function-name-face "#c0ffff")

  (set-face-foreground 'font-lock-variable-name-face "goldenrod")
  (set-face-foreground 'font-lock-type-face "darkorange")
  (set-face-foreground 'font-lock-constant-face "orange")
  (set-face-foreground 'font-lock-warning-face "red")

;;  (defface tex-verbatim)
;;  (make-face tex-verbatim)
;;  (set-face-attribute 'tex-verbatim "courier") ; no effect

  ;; Recognise operator-names and/bitand/bitor/compl/not/or/xor:
;;  (font-lock-add-keywords
;;   'c++-mode
;;   '("\\<\\(and\\|bitand\\|or\\|bitor\\|compl\\|not\\|xor\\)\\>"))


;; cf. http://hbfs.wordpress.com/2010/03/02/adding-keywords-in-emacs/
;;  (make-face 'font-lock-foo-face)
;;  (set-face-foreground 'font-lock-foo-face "violet")
;;
  (font-lock-add-keywords
   'c++-mode
   '(
     ("\\<\\(restrict\\)" 1 font-lock-keyword-face)
     ("\\<\\(return\\)" 1 font-lock-builtin-face)
;;     ("\\<\\(return\\)" 1 font-lock-foo-face);; does not work
     )
   )

  (setq c++-font-lock-extra-types
        '(
          ;;              "size_t" ;; not needed: *_t already built in
          "Complex" "complex"
          "ldouble" ;; long double
          "uint" "ulong" "ushort" "uchar" ;; unsigned
          "int32" "uint32" "int64" "uint64" ;; int types
          ;;              "string"
          ;;              "Integer" "Rational"    ;; GNU types
          "mod" ;; for mod
          "umod_t" "smod_t"
          "hfloat" "hfdata" "LIMB" ;; for hfloat
          "Type" "Type0" "Type1" "Type2" "Type3" "Type4" ;; for templates
          ;;              "F_Type" "I_Type" "SI_Type" "UI_Type" ;; for templates
          ) )

  (font-lock-add-keywords
   'c-mode
   '(
     ("\\<\\(return\\)" 1 font-lock-builtin-face)
     ("\\<\\(asm\\)" 1 font-lock-keyword-face)
     ) )

  (font-lock-add-keywords
   'latex-mode
   '(
     ("\\<\\(jjlabel\\)\\>" 1 font-lock-constant-face)
     ("\\<\\(jjformula\\)\\>" 1 font-lock-constant-face)
     ("\\<\\(jjvref\\)\\>" 1 font-lock-constant-face)
     ("\\<\\(jjVref\\)\\>" 1 font-lock-constant-face)
     ("\\<\\(jjref\\)\\>" 1 font-lock-constant-face)
     ("\\<\\(jjseqref\\)\\>" 1 font-lock-constant-face)
     ("\\<\\(jjpageref\\)\\>" 1 font-lock-constant-face)
     ;;
     ("\\<\\(jjcite\\)\\>" 1 font-lock-builtin-face)
     ("\\<\\(jjPcite\\)\\>" 1 font-lock-builtin-face)
     ("\\<\\(jjnew\\)\\>" 1 font-lock-builtin-face)
     ("\\<\\(jjxinput\\)\\>" 1 font-lock-builtin-face)
     ("\\<\\(jjindex\\)\\>" 1 font-lock-builtin-face)
     ("\\<\\(jjiseqindex\\)\\>" 1 font-lock-builtin-face)
     ;;
     ("\\<\\(jjignore\\)\\>" 1 font-lock-warning-face)
     ("\\<\\(begin\{figure\}\\)" 1 font-lock-builtin-face) ;;
     ("\\<\\(end\{figure\}\\)" 1 font-lock-builtin-face) ;;
     ("\\<\\(begin\{picture\}\\)" 1 font-lock-warning-face) ;;
     ("\\<\\(end\{picture\}\\)" 1 font-lock-warning-face) ;;
     ("\\<\\(begin\{tabular\}\\)" 1 font-lock-builtin-face) ;;
     ("\\<\\(end\{tabular\}\\)" 1 font-lock-builtin-face) ;;
     ("\\<\\(jjfile\\)\\>" 1 font-lock-warning-face prepend) ;; even inside comments
     ("\\<\\(jjdir\\)\\>" 1 font-lock-warning-face prepend) ;; even inside comments
     ("\\<\\(jjTODO\\)\\>" 1 font-lock-warning-face prepend) ;; even inside comments
     ("\\<\\(jjTodo\\)\\>" 1 font-lock-warning-face prepend) ;; even inside comments
     ("\\<\\(FloatBarrier\\)\\>" 1 font-lock-warning-face)
     ) )


  (font-lock-add-keywords ;; pari/gp
   'gp-script-mode
   '(
     ;;
     ("\\<\\(matrix\\)" 1 font-lock-variable-name-face)
     ("\\<\\(vector\\)" 1 font-lock-variable-name-face)
     ("\\<\\(vectorv\\)" 1 font-lock-variable-name-face)
     ;;
;;     ("\\<\\(matker\\)" 1 font-lock-variable-name-face)
;;     ("\\<\\(sigma\\)" 1 font-lock-variable-name-face)
;;     ("\\<\\(numdiv\\)" 1 font-lock-variable-name-face)
;;     ("\\<\\(divisors\\)" 1 font-lock-variable-name-face)
;;     ("\\<\\(divisors\\)" 1 font-lock-function-name-face)
;;     ("\\<\\(divisors\\)" 1 font-lock-type-face)
;;     ("\\<\\(divisors\\)" 1 sh-heredoc); no effect
     ;;
     ("\\<\\(return\\)" 1 font-lock-builtin-face)
     ("\\<\\(quit\\)" 1 font-lock-warning-face)
     ) )


  (global-set-key [(f7)] 'font-lock-fontify-buffer) ;; f7 for rehiliting
  (global-set-key [(control f7)] 'font-lock-mode) ;; toggle font-lock-mode
  (define-key esc-map [(f7)] 'font-lock-mode) ;; toggle font-lock-mode (outside X11)

  (setq jjj-flcfft 0) ;; font-lock-comment-face-foreground-toggle
  (global-set-key [(shift f7)] ;; (almost) hide comments
                  '(lambda () (interactive)
                     (progn
                       (if (eq 0 (setq jjj-flcfft (- 1 jjj-flcfft)))
                           (set-face-foreground 'font-lock-comment-face jjj-flcff1)
                         (set-face-foreground 'font-lock-comment-face jjj-flcff2)
                         ) ) ) )


  ;; no raising/lowering after underscore:
  (eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))

  )


;;;; following works only under X, no idea why...:
;;(load "dim-colors.el")
;;(defun jj-dim-colors ()
;;  (interactive)
;;  "Dim font-lock colors."
;;  (egoge-wash-out-fontlock-faces  .20) )
;;
;;(defun jj-brighten-colors ()
;;  (interactive)
;;  "Brighten font-lock colors."
;;  (egoge-wash-out-fontlock-faces -.20) )



;;(provide 'my-font-lock)

;;; my-font-lock.el ends here
