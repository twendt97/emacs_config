;;; keys.el --- key and menu settings

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-February-06 (17:36)
;;----------------------------------------------


;;(set-input-mode t nil t)
;; otherwise emacs doesn't believe that the terminal has a meta key.

;; (note that some more keys are are also defined in .jjelxwin.el)
;; cf. /usr/share/emacs/VERSION/lisp/loaddefs.el

;; for your convenience: the names of some keys
;; pause
;; up  down  left  right C-right M-right
;; end  home  insert  delete
;; next  prior
;; kp-left 4  kp-right 6
;; kp-up 8  kp-down 2
;; kp-next 3  kp-prior 9
;; kp-home 7
;; kp-begin 5
;; kp-delete .
;; kp-insert 0
;; kp-multiply  kp-add  kp-subtract kp-divide
;; kp-enter

;; A useful web page is
;;   http://www.nongnu.org/emacs-tiny-tools/keybindings/index-body.html


;;(defun jjj-overwrite-mode ()
;;  ""
;;  (interactive)
;;  (progn
;;    ()
;;    (overwrite-mode nil) ) )



;;; for [(shift insert)]  see xwin.el

;; Free [(control x) (f)] from set-fill-column.
;; I hit this one too often by error,
;; and "M-x set-fill-column" isn't that complicated.
(global-unset-key [(control x) (f)])

(global-unset-key [(control q)])  ;; used as prefix for own commands
(global-set-key [(control q) (control q)] 'quoted-insert)  ;; twice to insert funny characters
(global-unset-key [(insert)])  ;; use kp-insert instead
;;(global-set-key [(meta insert)]  '(lambda () (interactive) (overwrite-mode nil)))

 ;; nicer 'page-up/down' keys:
(global-set-key [(next)]     '(lambda () (interactive) (progn (forward-line 30) (recenter) )))
(global-set-key [(kp-next)]  '(lambda () (interactive) (progn (forward-line 30) (recenter) )))
(global-set-key [(prior)]    '(lambda () (interactive) (progn (forward-line -30) (recenter) )))
(global-set-key [(kp-prior)] '(lambda () (interactive) (progn (forward-line -30) (recenter) )))

;; nicer left/right scrolling:
;;(global-set-key [C-next]  '(lambda () (interactive) (scroll-left 20)))
;;(global-set-key [C-prior] '(lambda () (interactive) (scroll-right 20)))
(global-set-key [(control kp-right)]  '(lambda () (interactive) (scroll-left 10)))
(global-set-key [(control kp-left)] '(lambda () (interactive) (scroll-right 10)))

;; intead of the annoying jump to begin/end of buffer
;; goto begin/end of line:
(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(end)]  'end-of-line)
(global-set-key [(meta home)] 'beginning-of-line-text)
(global-set-key [(meta end)]  'end-of-line)
;;
;; and make the beg/end-buffer available through ...
;;(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [(control home)]  '(lambda () (interactive) (goto-char (point-min))))
;;(global-set-key [C-end]   'end-of-buffer)
(global-set-key [(control end)]   '(lambda () (interactive) (goto-char (point-max))))
;; and for text terminal ...
(define-key esc-map [(home)] '(lambda () (interactive) (goto-char (point-min))))
(define-key esc-map [(end)]  '(lambda () (interactive) (goto-char (point-max))))

;; this should be default anyway:
(global-set-key [(meta left)]  'backward-word)
(global-set-key [(control left)]  'backward-word)
(global-set-key [(meta right)] 'forward-word)
(global-set-key [(control right)] 'forward-word)
;;(global-set-key [M-\[1\;3C] 'forward-word)
;;(global-set-key [M-\[1\;3D] 'backward-word)
;; ESC [ 1 ; 3 D C-h l


(global-set-key [(meta down)] '(lambda () (interactive) (progn (forward-line 4) (recenter) ) ))
(global-set-key [(meta up)]   '(lambda () (interactive) (progn (forward-line -4) (recenter) ) ))
;; wheel mouse (vertical scrolling):
(global-set-key [(mouse-5)] '(lambda () (interactive) (progn (forward-line 4) (recenter) ) ))
(global-set-key [(mouse-4)] '(lambda () (interactive) (progn (forward-line -4) (recenter) ) ))
(global-set-key [(meta mouse-5)] '(lambda () (interactive) (progn (forward-line 1) (recenter) ) ))
(global-set-key [(meta mouse-4)] '(lambda () (interactive) (progn (forward-line -1) (recenter) ) ))
(global-set-key [(control mouse-5)] '(lambda () (interactive) (progn (forward-paragraph 1) (recenter) ) ))
(global-set-key [(control mouse-4)] '(lambda () (interactive) (progn (forward-paragraph -1) (recenter) ) ))

(global-set-key [(meta control mouse-4)] '(lambda () (interactive) (previous-buffer) ))
(global-set-key [(meta control mouse-5)] '(lambda () (interactive) (next-buffer) ))



;;(global-set-key [(meta s)]  'jj-search-marked-text)

;;; function-keys:
;; watchout for stupid window manager configurations
;; that steal a lot of keys (e.g. C-f4 for 'kill window')

(global-set-key [(control f1)] 'find-file-at-point)
(define-key esc-map [(f1)] 'jj-load-dotemacs)
(global-set-key [(f1)] 'help-for-help) ;; help
(global-set-key [(f2)] 'repeat-complex-command) ;; '2.time'
(global-set-key [(f3)] 'find-file-read-only) ;; 'view'
(global-set-key [(control f3)] 'jj-reopen-file-read-only) ;; 'view'

;;(global-set-key [(f4)] 'find-file)      ;; 'edit'
(global-set-key [(f4)] 'jjj-match-paren)
;;(define-key global-map [(shift f4)] 'jjj-match-paren)  ;;
;;(define-key global-map [(control f4)] 'jjj-match-paren)
(define-key esc-map [(f4)] 'jjj-match-paren)

;; plain-f5 set mode specific: see hooks.el
;;(global-set-key [(f5)] 'jj-indent-next-c-func)
(global-set-key [(control f5)] 'jj-reopen-file)
(global-set-key [(meta f5)] 'jj-reopen-file-too)
;;(define-key esc-map [(f5)] 'jj-search-region)

(global-set-key [(f6)] 'switch-to-buffer) ;; like 'control-x b'
(global-set-key [(control f6)] 'buffer-menu)
(global-set-key [(shift f6)] 'buffer-menu-other-window)

;; f7, shift-f7 and control-f7 are used for font-lock-mode
(global-set-key [(meta f7)] 'shell-script-mode)

(global-set-key [(f8)] 'kill-buffer)
(global-set-key [(control f8)] 'kill-this-buffer)
;;(global-set-key [M-f8] 'delete-other-windows)
(global-set-key [(meta f8)] '(lambda () (interactive) (progn (delete-other-windows) (recenter))))

;; Toggle display of the menu bar on each frame:
(global-set-key [(f9)] 'menu-bar-mode)  ;; toggle menu-bar visible/invisible
;;(define-key global-map [(control f9)] ')
(define-key esc-map [(f9)] 'ispell-buffer)
(global-set-key [(meta f9)] 'delete-window)

(define-key esc-map [(f10)] 'ispell-comments-and-strings)
;; Text-mode emulation of looking and choosing from a menubar:
(global-set-key [(f10)] 'tmm-menubar)

(global-set-key [(f11)] 'goto-column)
(global-set-key [(control f11)] 'save-buffer)
(global-set-key [(Scroll_Lock)] 'save-buffer)
;;(define-key esc-map [(f11)] 'emacs-lisp-byte-compile)
;;(define-key esc-map [(f11)] 'jjj-byte-compile-my-files)

;;(global-set-key [(control f12)] 'goto-column)
(global-set-key [(f12)] 'goto-line)

(global-set-key [(control f12)]  'jjj-save-and-compile)
(global-set-key [(meta f12)] 'jjj-save-and-compile2)

;;(define-key esc-map [(f12)] '(lambda () (interactive) (progn (kill-compilation) (jjj-save-and-compile) )))
(define-key esc-map [(f12)] 'jjj-save-and-compile)
;;(define-key esc-map [(control f12)] 'jjj-write-project)

;;(global-set-key [(control c) (l)] 'goto-line)
;;(global-set-key [(control c) (g)] 'goto-line)


;;(global-set-key [(control x) (B)] 'buffer-menu)
(global-set-key [(control x) (B)] 'ibuffer); even better!
;;(global-set-key [(control x) (b)] 'ibuffer); ... so we bind that one as well
;;(defalias 'list-buffers 'ibuffer); ... and get rid of list-buffers

(global-set-key [(control tab)] "    ")
;;(global-set-key [(control shift tab)] "hello")


;; disable zap-char:
;;(global-set-key [?\M-z] nil)
(define-key esc-map [(z)] nil)

;;(global-set-key [C-%] 'query-replace)
;;(global-set-key [?\M-%] 'query-replace)
(global-set-key [(meta %)] 'query-replace)
;;(define-key esc-map "%" 'query-replace)

;;(global-set-key "\C-^" 'isearch-forward-regexp)
;;(global-set-key [?\M-^] 'query-replace-regexp)
(global-set-key [(meta ^)] 'query-replace-regexp)
;;(global-set-key "\C-cq" 'query-replace-regexp)

;;(global-set-key [C-&] 'replace-regexp)
;;(global-set-key [?\M-&] 'replace-regexp)
;;(global-set-key "\C-cr" 'replace-regexp)

(global-set-key [(kp-enter)] [?\C-\j])  ;; newline + indent


(global-set-key [(meta pause)] 'jjj-delete-line)
(global-set-key [(control pause)] 'jjj-kill-ring-save-line)
(global-set-key [(pause)] 'jjj-duplicate-line)

;;(global-set-key "\C-cp" '(lambda () (interactive) (check-parens)))

;; german umlauts with US keyboard:
(global-set-key [(control q) (u) (a)] '(lambda () (interactive) (insert "ä")))
(global-set-key [(control q) (u) (A)] '(lambda () (interactive) (insert "Ä")))
(global-set-key [(control q) (u) (o)] '(lambda () (interactive) (insert "ö")))
(global-set-key [(control q) (u) (O)] '(lambda () (interactive) (insert "Ö")))
(global-set-key [(control q) (u) (u)] '(lambda () (interactive) (insert "ü")))
(global-set-key [(control q) (u) (U)] '(lambda () (interactive) (insert "Ü")))
(global-set-key [(control q) (u) (s)] '(lambda () (interactive) (insert "ß")))
;; same with PrintScreen key:
(global-set-key [(print) (a)] '(lambda () (interactive) (insert "ä")))
(global-set-key [(print) (A)] '(lambda () (interactive) (insert "Ä")))
(global-set-key [(print) (o)] '(lambda () (interactive) (insert "ö")))
(global-set-key [(print) (O)] '(lambda () (interactive) (insert "Ö")))
(global-set-key [(print) (u)] '(lambda () (interactive) (insert "ü")))
(global-set-key [(print) (U)] '(lambda () (interactive) (insert "Ü")))
(global-set-key [(print) (s)] '(lambda () (interactive) (insert "ß")))

(global-set-key [(control q) (t)] 'jjj-insert-date-time)
(global-set-key [(control q) (d)] 'jjj-insert-date)
(global-set-key [(control q) (o)] 'jjj-insert-oeis-sig)
(global-set-key [(control q) (F)] 'jjj-insert-path-name)
(global-set-key [(control q) (f)] 'jjj-insert-file-name)
(global-set-key [(control q) (b)] 'jjj-insert-base-name)
(global-set-key [(control q) (r)] 'jjj-insert-rand128)
;;(global-set-key "\C-qt" 'jjj-insert-date-time)
;;(global-set-key "\C-qd" 'jjj-insert-date)
;;(global-set-key "\C-qf" 'jjj-insert-file-name)
;;(global-set-key "\C-qb" 'jjj-insert-base-name)
;;(global-set-key "\C-qr" 'jjj-insert-rand128)


;;(provide 'keys)

;;; keys.el ends here
