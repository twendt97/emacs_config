;;; hooks.el --- hooks and specific settings for certain modes

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-March-23 (18:28)
;;----------------------------------------------


;;; Startup:
;; http://askubuntu.com/questions/4820/keeping-emacs-from-splitting-the-window-when-openning-multiple-files
;; The emacs-startup-hook is run after loading the init file and
;; processing the command line, so all files have been loaded and
;; opened; calling delete-other-windows leaves just one of them
;; visible (normally the last one given on the command line).
(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)


;; C/C++:
;;
;; https://www.emacswiki.org/emacs/IndentingC
;; C-indentation style:
;;(setq c-echo-syntactic-information-p t)
(defconst c-basic-offset 4)
(c-set-offset 'defun-block-intro 4)
(c-set-offset 'statement-block-intro 4)
(c-set-offset 'statement-cont 0)
(c-set-offset 'func-decl-cont 0)
(c-set-offset 'block-open 0)
(c-set-offset 'block-close 0)
(c-set-offset 'substatement-open 0)
(c-set-offset 'substatement 4)
(c-set-offset 'namespace-open 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'namespace-close 0)
;; try:  M-x describe-variable c-offsets-alist
;;
;; C-C C-o to determine syntactic element seen.
;; To re-indent whole file use  C-x h C-M-\
;;
(defun my-c-mode-common-hook ()
  (progn
    ;;(flyspell-prog-mode)

    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "//")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "//")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-block-comment "/*" "*/")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-block-comment "/*" "*/")))
    (local-set-key [(control x) (\,)] '(lambda () (interactive) (jjj-indent-with-comment "//")))
    (local-set-key [(control q) (/)] '(lambda () (interactive)
                                  (insert "// -------------------------\n")))
    (local-set-key [(f5)] 'jj-indent-next-c-func)

;;    (local-set-key [C-M-tab] 'ispell-complete-word)
    (local-set-key [(meta tab)] 'ispell-complete-word)

    (local-set-key [(control x) (c)] '(lambda () (interactive) (copyright-update nil nil)))

;;    (setq c-old-style-variable-behavior t)
;;    (c-set-style "K&R")
;;    (c-toggle-auto-state 1)
    (c-toggle-hungry-state 1)
    (font-lock-mode 1)
    ) )
;;
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;(add-hook 'c++-mode-hook 'my-c-mode-common-hook)


;;(defun jjj-font-lock-unless-empty ()
;;  "set font-lock-mode unless file is empty (new)"
;;  (interactive)
;;  (if (eq (point-min) (point-max))
;;      ()
;;    (font-lock-mode 1) )
;;  )

;; MAKEFILE:
(defun my-makefile-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "# ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "# ")))
;;    (font-lock-mode 1)
    ))
;;
(setq makefile-mode-hook 'my-makefile-mode-hook)

;; CONF:
;; e.g. /etc/hosts
(defun my-conf-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "# ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "# ")))
    ) )
;;
(setq conf-mode-hook 'my-conf-mode-hook)


;; TEXT:
(defun my-text-mode-hook ()
  (progn
    (auto-fill-mode 0)

    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "> ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "> ")))
    (local-set-key [(control x) (g)] '(lambda () (interactive) (ispell-change-dictionary "german")) )
    (local-set-key [(control x) (e)] '(lambda () (interactive) (ispell-change-dictionary "american-w_accents")) )
    (local-set-key [(control q) (g) (g)] "Mit besten Gruessen,   Joerg Arndt\n")
    (local-set-key [(control q) (g) (e)] "Best regards,   Joerg Arndt\n")
    (local-set-key [(control q) (g) (j)] "Best regards,   jj\n")
    ) )
;;
(setq text-mode-hook 'my-text-mode-hook)


(defun my-maple-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "# ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "# ")))
    (font-lock-mode 1)
    ) )
;;
(setq maple-mode-hook 'my-maple-mode-hook)


;; TeX/LaTeX:
(setq-default TeX-auto-regexp-list 'LaTeX-auto-minimal-regexp-list)
(setq-default TeX-auto-parse-length 2000)
(setq TeX-auto-untabify t)

;; flyspell-mode grabs key control-comma
;; The following lines fixes that
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") nil))

(defun my-tex-mode-hook ()
  (progn
    (setq fill-column 80)

    (setq electric-indent-mode nil);; no magic here, thanks

    (setq ispell-parser 'tex)
    (flyspell-mode)

    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "%")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "%")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "% ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "% ")))
    (local-set-key [(tab)] "    ")

    (local-set-key [(control =)]  '(lambda () (interactive) (insert "  & = &  ")))
    (local-set-key [(meta control =)]  '(lambda () (interactive) (insert "  \\;=\\;  ")))
;;    (local-set-key [(control \;)]  '(lambda () (interactive) (insert "  & := &  ")))
;;    (local-set-key [(control :)]  '(lambda () (interactive) (insert "  \\;:=\\;  ")))

;;    (local-set-key [(control c) (v)] 'tex-validate-buffer)

    (local-set-key [(control q) (\()] '(lambda () (interactive) (insert "\\left(")))
    (local-set-key [(control q) (9)] '(lambda () (interactive) (insert "\\left(")))
    (local-set-key [(control q) (0)] '(lambda () (interactive) (insert "\\right)")))
    (local-set-key [(control q) (\))] '(lambda () (interactive) (insert "\\right)")))
    (local-set-key [(control q) (\[)] '(lambda () (interactive) (insert "\\left[")))
    (local-set-key [(control q) (\])] '(lambda () (interactive) (insert "\\right]")))
    (local-set-key [(control q) (\{)] '(lambda () (interactive) (insert "\\left\\{")))
    (local-set-key [(control q) (\})] '(lambda () (interactive) (insert "\\right\\}")))
    (local-set-key [(control q) (.)] '(lambda () (interactive) (insert "\\cdot{}")))
    (local-set-key [(control q) (x) (b)] 'jjj-tex-insert-bibitem)
    (local-set-key [(control q) (x) (B)] 'jjj-tex-insert-bibitem-b)
    (local-set-key [(control q) (x) (l)] 'jjj-tex-insert-listing)
    (local-set-key [(control q) (x) (c)] 'jjj-tex-insert-code)
    (local-set-key [(control q) (x) (e)] 'jjj-tex-insert-eqn)
    (local-set-key [(control q) (x) (s)] 'jjj-tex-insert-subeqn)
    (local-set-key [(control q) (x) (f)] 'jjj-tex-insert-figure)

    (local-set-key [(control q) (x) (t)]
                   (lambda () (interactive)
                     (progn (insert "\\jjTodo{Z:  }") (backward-char 2) ) ) )
    (local-set-key [(control q) (x) (T)]
                   (lambda () (interactive)
                     (progn (insert "\\jjTODO{Z:  }") (backward-char 2) ) ) )

    (local-set-key [(control q) (x) (d)]
                   (lambda () (interactive)
                     (progn (insert "\\jjDone{Z:  }") (backward-char 2) ) ) )


;;    (local-set-key [C-f9]  'jjj-TeX-compile)

    ;; "esc-mouse-1" ==  [27 mouse-1]: show pdf
    (local-set-key [(27) (mouse-1)] '(lambda (event) (interactive "e")
                                   (progn (mouse-set-point event)
                                          (jj-pdf-at-pos) ) ) )

    ;; make esc-esc-mouse-1:  show dvi
    (local-set-key [(27) (27) (mouse-1)] '(lambda (event) (interactive "e")
                                   (progn (mouse-set-point event)
                                          (jj-xdvi-at-pos) ) ) )

    ;; Umlauts etc. with US keyboards:
    (local-set-key [(control q) (u) (a)] "\\\"{a}")
    (local-set-key [(control q) (u) (A)] "\\\"{A}")
    (local-set-key [(control q) (u) (o)] "\\\"{o}")
    (local-set-key [(control q) (u) (O)] "\\\"{O}")
    (local-set-key [(control q) (u) (u)] "\\\"{u}")
    (local-set-key [(control q) (u) (U)] "\\\"{U}")
    (local-set-key [(control q) (u) (s)] "\\ss{}")
    (local-set-key [(control q) (u) (e)] "\\'{e}")
    (local-set-key [(control q) (u) (E)] "\\'{E}")
    (local-set-key [(control q) (u) (c)] "\\'{c}")
    (local-set-key [(control q) (u) (C)] "\\c{c}")
    (local-set-key [(control q) (u) (i)] "{\\'\\i}")
    (local-set-key [(control q) (u) (l)] "{\\l}")
    (local-set-key [(control q) (u) (n)] "{\\~n}")
    ;; same with PrintScreen key:
    (local-set-key [(print) (a)] "\\\"{a}")
    (local-set-key [(print) (A)] "\\\"{A}")
    (local-set-key [(print) (o)] "\\\"{o}")
    (local-set-key [(print) (O)] "\\\"{O}")
    (local-set-key [(print) (u)] "\\\"{u}")
    (local-set-key [(print) (U)] "\\\"{U}")
    (local-set-key [(print) (s)] "\\ss{}")
    (local-set-key [(print) (e)] "\\'{e}")
    (local-set-key [(print) (E)] "\\'{E}")
    (local-set-key [(print) (c)] "\\'{c}")
    (local-set-key [(print) (C)] "\\c{c}")
    (local-set-key [(print) (i)] "{\\'\\i}")
    (local-set-key [(print) (l)] "{\\l}")
    (local-set-key [(print) (n)] "{\\~n}")

;;    (print "TeX umlauts loaded: ctrl-c-a:=\"a ctrl-c-s:=\"s etc.")

    ;; for German keyboards:
;;    (local-set-key "ä" "\"a")
;;    (local-set-key "ö" "\"o")
;;    (local-set-key "ü" "\"u")
;;    (local-set-key "Ä" "\"A")
;;    (local-set-key "Ö" "\"O")
;;    (local-set-key "Ü" "\"U")
;;    (local-set-key "ß" "\"s")

    (local-unset-key [(\")])  ;; prevent expansion into ''


;;    (font-lock-mode 1)

;;    ;; put "listing" into the tex-verbatim-environments:
;;    (setq tex-verbatim-environments '("verbatim" "verbatim*" "listing"))
;;    ;; ... and refresh tex-font-lock-syntactic-keywords: ...
;;    ;; (identically to tex-mode.el)
;;    (setq tex-font-lock-syntactic-keywords
;;          (let ((verbs (regexp-opt tex-verbatim-environments t)))
;;            `(
;;              (,(concat "^\\\\begin *{" verbs "}.*\\(\n\\)") 2 "|")
;;              (,(concat "^\\(\\\\\\)end *{" verbs "}\\(.?\\)") (1 "|") (3 "<"))
;;              ("\\\\verb\\**\\([^a-z@*]\\)"
;;               1 (tex-font-lock-verb (match-end 1) ) ) )
;;            ) )

    ;;; following done in .emacs (i.e. the ugly way):
;;    (set-face-attribute 'subscript nil :height '1.0)
;;    (set-face-attribute 'superscript nil :height '1.0)
;;    (set-face-font 'superscript (default))
;;    (set-face-font 'subscript (default))
;;    (set-face-font 'tex-verbatim (default))
    ;; ... together with: ...
;;    (defun tex-font-lock-suscript (pos)
;;      (unless (or (memq (get-text-property pos 'face)
;;                        '(font-lock-constant-face
;;                          font-lock-builtin-face
;;                          font-lock-comment-face
;;                          tex-verbatim))
;;                  ;; Check for backslash quoting
;;                  (let ((odd nil)
;;                        (pos pos))
;;                    (while (eq (char-before pos) ?\\)
;;                      (setq pos (1- pos) odd (not odd)))
;;                    odd))
;;        (if (eq (char-after pos) ?_)
;;            '(face subscript display (raise -0.0)) ; was -0.3
;;          '(face superscript display (raise +0.0))))) ; was +0.3

    ) )
;; used in:
(setq latex-mode-hook 'my-tex-mode-hook)
(setq TeX-mode-hook 'my-tex-mode-hook)

;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;(setq reftex-plug-into-AUCTeX t)


;; METAPOST:
(defun my-mpost-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "%")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "%")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "% ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "% ")))
;;    (local-set-key [(tab)] "    ")
 ) )
;; used in:
(setq meta-common-mode-hook 'my-mpost-mode-hook)


;; SHELL-SCRIPT: (cf. sh-script.el)
(defun my-sh-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "# ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "# ")))
;;    (if (eq window-system 'x)
;;        (progn
;;          (setq font-lock-maximum-decoration t)
;;          (font-lock-mode)
;;          ) )
;;    (font-lock-mode 1)
    ) )
;;
(setq sh-mode-hook 'my-sh-mode-hook)


;; PERL:
(defun my-perl-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control x) (\,)] '(lambda () (interactive) (jjj-indent-with-comment "#")))
    ;;(local-set-key [(tab)] "    ")
    ;;(local-set-key [(tab)] 'indent-according-to-mode)
    (local-set-key [(tab)] 'indent-for-tab-command)
    ;;(setq paren-sexp-mode nil)
;;    (font-lock-mode 1)
    ) )
;;
(setq perl-mode-hook 'my-perl-mode-hook)


;; ELISP:
(defun my-elisp-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment ";;")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment ";;")))
    (local-set-key [(f5)] 'jj-indent-next-el-func)
    (local-set-key [(control x) (\,)] '(lambda () (interactive) (jjj-indent-with-comment ";;")))
    (local-set-key [(tab)] 'indent-for-tab-command)
;;    (font-lock-mode 1)
  ) )
;;
(setq emacs-lisp-mode-hook 'my-elisp-mode-hook)




;; MUPAD:
(defun my-mupad-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "//")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "//")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-block-comment "/*" "*/")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-block-comment "/*" "*/")))
    (setq mupad-indent-level 4)

    (set-face-foreground 'mupad-font-lock-comment-face "brown")
;;    (set-face-foreground 'font-lock-comment-face jjj-flcff2)
    (local-set-key [(shift f7)] ;; (almost) hide comments
                   '(lambda () (interactive)
                      (progn
                        (if (eq 0 (setq jjj-flcfft (- 1 jjj-flcfft)))
                            (set-face-foreground 'mupad-font-lock-comment-face jjj-flcff1)
                          (set-face-foreground 'mupad-font-lock-comment-face jjj-flcff2)
                          ) ) ) )

    (set-face-foreground 'mupad-font-lock-string-face "forestgreen")

;;    (set-face-foreground 'mupad-font-lock-keyword-face "MediumBlue")
    (set-face-foreground 'mupad-font-lock-keyword-face "DarkBlue")
;;    (set-face-foreground 'mupad-font-lock-keyword-face "grey33")

    (set-face-foreground 'mupad-font-lock-type-face "darkorange") ; DOM_XYZ
    (set-face-foreground 'mupad-font-lock-info-face "darkmagenta")
    (set-face-foreground 'mupad-font-lock-options-face "red")
    (set-face-foreground 'mupad-font-lock-domain-face "goldenrod")

;;    (set-face-foreground 'mupad-font-lock-function-name-face "darkviolet")
    (set-face-foreground 'mupad-font-lock-function-name-face "darkgreen")
    (set-face-background 'mupad-font-lock-function-name-face "gray90")
    (set-face-underline-p 'mupad-font-lock-function-name-face t)
;;    (set-face-foreground 'mupad-font-lock-function-name-face "black")
;;    (set-face-background 'mupad-font-lock-function-name-face "#c0ffff")
;;    (font-lock-mode nil)  ;; font-lock-mode causes big slowdown

;;    (set-face-foreground 'mupad-font-lock-variable-name-face "goldenrod")

;;    (local-set-key "(" '(lambda () (interactive) (insert "(")))
;;    (local-set-key "{" '(lambda () (interactive) (insert "{")))
;;    (local-set-key "[" '(lambda () (interactive) (insert "[")))
;;    (local-set-key "\"" '(lambda () (interactive) (insert "\"")))
    ) )
;;
(add-hook 'mupad-mode-hook 'my-mupad-mode-hook)


;; PARI/GP:
;;
;; NOTE: the pari mode is intrusive, annoying, and broken beyond repair.
;; Get used to random messages like (on pressing the tab key):
;;   (sli-electric-tab): (wrong-type-argument integer-or-marker-p nil)
;; Apparently braindead "features" (like redefinition
;; of standard keys to utter nonsense) cannot be avoided.
;; Note the mode kills case-fold-search, you may want
;; to fix that by commenting out the line (setq case-fold-search nil)
;; in the file pari.el
;; Further comment out the line (fset 'x-defined-colors nil)
;; in the file pari-fontification.el to avoid random failures.
;;
;; The code below is an attempt to avoid the worst stupidity.
;;
;; No fixes for the following:
;; Word boundaries are redefined (alt-backspace kills over
;; dots and underscores, alt-left/right ignores same chars):
;; comment out the line with the comment ;; word constituent
;; Keyword completion is dead.
;;
;;(defun my-pari-mode-hook ()
(setq gp-script-mode-hook  (function (lambda ()
  (progn
    (define-key gp-script-map [(control \,)]
      '(lambda () (interactive) (jjj-insert-comment "\\\\") ) )

    (define-key gp-script-map [(control <)]
      '(lambda () (interactive) (jjj-delete-comment "\\\\") ) )

    (define-key gp-script-map [(control .)]
      '(lambda () (interactive) (jjj-insert-block-comment "/*" "*/") ) )

    (define-key gp-script-map [(control >)]
      '(lambda () (interactive) (jjj-delete-block-comment "/*" "*/") ) )

    (define-key gp-script-map [(control q) (/)] '(lambda () (interactive)
                                                   (insert " /* ----- */\n")))

;;    ;; avoid broken sli-electric-tab:
    (local-set-key [(tab)] "    ")
    (define-key gp-script-map [(tab)] '(lambda () (interactive) (insert "    ") ) )
;;    (define-key gp-script-map [(tab)] 'indent-for-tab-command)
;;    (local-set-key [(tab)] 'indent-for-tab-command)
;;    (setq gp-indent-level 4); "Indentation used after \"{\"."


;;    (define-key gp-script-map [?\C-l]
;;      '(lambda () (interactive) (recenter) (font-lock-fontify-buffer) ) )

    ;; incremental search case insensitive if only small letters typed:
;;    (setq-local case-fold-search t)  ;; triggers error message
;;    (eval '(lambda () (setq case-fold-search t)))  ;; no effect, see NOTE above
    ;; ... so we have to change file pari.el function: gp-learn-sexp

;;    (setq-local gp-language 'english) ;; no effect

    ;; ctrl-l shall also recenter:
;;    (define-key gp-script-map "\C-l"
    (define-key gp-script-map [(control l)]
      '(lambda () (interactive)
         (progn (recenter) (gp-update-fontification)) ) )

;;    (fset 'x-defined-colors nil);; bug in pari-fontification.el

    ;; they even redefined that one:
    (define-key gp-script-map [(meta \\)] 'delete-horizontal-space)

    (define-key gp-script-map [(control q) (x) (f)] 'jjj-insert-pari-func)

;;    (defconst jjj-test-pari-hook 239)
;;    (message "pari: %d" jjj-test-pari-hook)
;;    ) )

;;    (BOOM--pari-mode-is-broken-beyond-repair)
    ) ) ) )
;;
;;
(setq gp-mode-hook
      (function (lambda ()
                  (progn
                    (setq gp-language 'english)
;;                  (local-set-key [(tab)] "    ") ;; avoid broken sli-electric-tab
      ) ) ) )
;;
;;
(setq pari-mode-hook
      (function (lambda ()
                  (progn
                    (setq gp-language 'english)
;;                  (local-set-key [(tab)] "    ") ;; avoid broken sli-electric-tab
;;                  (setq-local case-fold-search t)
      ) ) ) )
;;
;;

;; RUBY:
(defun my-ruby-mode-hook ()
  (progn
    ;; nothing so far
    ) )
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)


;; HTML:
(defun my-html-mode-hook ()
  (progn
    ;; note: preview (aka browse-url-of-buffer) is ctrl-c ctrl-v
    (local-set-key [(control .)] '(lambda () (interactive)
                               (jjj-insert-block-comment "<!--" "-->")))
    (local-set-key [(control >)] '(lambda () (interactive)
                               (jjj-delete-block-comment "<!--" "-->")))

    ;; umlauts for us-keyboards:
    (local-set-key [(control q) (u) (a)] "&auml;")
    (local-set-key [(control q) (u) (o)] "&ouml;")
    (local-set-key [(control q) (u) (u)] "&uuml;")
    (local-set-key [(control q) (u) (A)] "&Auml;")
    (local-set-key [(control q) (u) (O)] "&Ouml;")
    (local-set-key [(control q) (u) (U)] "&Uuml;")
    (local-set-key [(control q) (u) (s)] "&szlig;")

    (local-set-key [(meta tab)] 'ispell-complete-word)

    ;; umlauts for german keybords:
;;    (local-set-key "ä" "&auml")
;;    (local-set-key "ö" "&ouml")
;;    (local-set-key "ü" "&uuml")
;;    (local-set-key "Ä" "&Auml")
;;    (local-set-key "Ö" "&Ouml")
;;    (local-set-key "Ü" "&Uuml")
;;    (local-set-key "ß" "&szlig;")

;;    (print "HTML umlauts loaded: ctrl-c-a:=&auml; ctrl-c-s:=&szlig; etc.")
    ) )
;;
(add-hook 'html-helper-mode-hook 'my-html-mode-hook)
(add-hook 'html-mode-hook 'my-html-mode-hook)


;; MAIL:
(defun my-mail-mode-hook ()
  (progn
;;    (auto-fill-mode 0)
    (flyspell-mode)
    (setq fill-column 70)
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "> ")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "> ")))
;;    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "> ")))
;;    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "> ")))
    (local-set-key [(control q) (g) (g)] "Mit besten Gruessen,   Joerg Arndt\n")
    (local-set-key [(control q) (g) (e)] "Best regards,   Joerg Arndt\n")
    (local-set-key [(control q) (g) (j)] "Best regards,   jj\n")
    (local-set-key [(control x) (g)] '(lambda () (interactive) (ispell-change-dictionary "german")) )
;;    (local-set-key [(control x) (g)] '(lambda () (interactive) (ispell-change-dictionary "de-neu")) )
    (local-set-key [(control x) (e)] '(lambda () (interactive) (ispell-change-dictionary "american-w_accents")) )
    ) )
;;
(setq mail-mode-hook 'my-mail-mode-hook)

;; Postscript:
(defun my-ps-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "\%")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "\%")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "\% ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "\% ")))
    ) )
;;
(add-hook 'ps-mode-hook 'my-ps-mode-hook)


;; Octave:
(defun my-octave-mode-hook ()
  (progn
    (local-set-key [(control \,)] '(lambda () (interactive) (jjj-insert-comment "#")))
    (local-set-key [(control <)] '(lambda () (interactive) (jjj-delete-comment "#")))
    (local-set-key [(control .)] '(lambda () (interactive) (jjj-insert-comment "# ")))
    (local-set-key [(control >)] '(lambda () (interactive) (jjj-delete-comment "# ")))
    (setq comment-column 0)
    ) )
;;
(add-hook 'octave-mode-hook 'my-octave-mode-hook)


;;(defun my-signal-USR1-hook ()  (progn (print "USR1") ) )
;;(add-hook 'signal-USR1-hook 'my-signal-USR1-hook)
;;(setq signal-USR1-hook 'font-lock-mode)


;;(provide 'hooks)

;;; hooks.el ends here
