;;; fonts.el --- font selection from the menu bar

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:09)
;;----------------------------------------------


(require 'menu-bar)


;; add entry to menu-bar:
(defvar menu-bar-jj-fonts (make-sparse-keymap "jjfonts"))
(define-key global-map [menu-bar jjfonts] (cons "jj-fonts" menu-bar-jj-fonts))

;; Menu entries (the numbers [item-NN] do NOT affect the order):
;;
;; First entry here is lowest in menu.
;; Last is top entry in menu:
(define-key menu-bar-jj-fonts [item-000]
  '("Mono-16:bold" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-16:bold") )) )
(define-key menu-bar-jj-fonts [item-010]
  '("Mono-16" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-16") )) )

(define-key menu-bar-jj-fonts [item-100]
  '("Mono-14:bold" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-14:bold") )) )
(define-key menu-bar-jj-fonts [item-110]
  '("Mono-14" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-14") )) )

(define-key menu-bar-jj-fonts [item-200]
  '("Mono-12:bold" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-12:bold") )) )
(define-key menu-bar-jj-fonts [item-210]
  '("Mono-12" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-12") )) )

(define-key menu-bar-jj-fonts [item-300]
  '("Mono-10:bold" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-10:bold") )) )
(define-key menu-bar-jj-fonts [item-310]
  '("Mono-10" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-10") )) )

(define-key menu-bar-jj-fonts [item-400]
  '("Mono-8:bold" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-8:bold") )) )
(define-key menu-bar-jj-fonts [item-410]
  '("Mono-8" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-8") )) )

(define-key menu-bar-jj-fonts [item-500]
  '("Mono-6:bold" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-6:bold") )) )
(define-key menu-bar-jj-fonts [item-510]
  '("Mono-6" . (lambda () (interactive) (set-default-font "DejaVu Sans Mono-6") )) )

(define-key menu-bar-jj-fonts [item-600]
  '("10x20" . (lambda () (interactive) (set-default-font "10x20") )) )
(define-key menu-bar-jj-fonts [item-610]
  '("8x13bold" . (lambda () (interactive) (set-default-font "8x13bold") )) )


;;(provide 'menu)

;;; fonts.el ends here
