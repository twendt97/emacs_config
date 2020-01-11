;;; menu.el --- an extra entry for the menu bar

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:08)
;;----------------------------------------------


(require 'menu-bar)


;; add entry to menu-bar:
(defvar menu-bar-jj-menu (make-sparse-keymap "jjmenu"))
(define-key global-map [menu-bar jjmenu] (cons "jj-funcs" menu-bar-jj-menu))

;; Menu entries (the numbers [item-NN] do NOT affect the order):
;;
;; First entry here is lowest in menu.
(define-key menu-bar-jj-menu [item-99]
  '("copyright-update" . copyright-update))

(define-key menu-bar-jj-menu [item-101]
  '("highline-mode" . highline-mode))

(define-key menu-bar-jj-menu [item-102]
  '("ascii-check" . ascii-check))

(define-key menu-bar-jj-menu [item-103]
  '("ispell-buffer" . ispell-buffer))

(define-key menu-bar-jj-menu [item-105]
  '("htmlize buffer" . htmlize-buffer))

(define-key menu-bar-jj-menu [item-106]
  '("htmlize region" . htmlize-region))

(define-key menu-bar-jj-menu [item-108]
  '("color-ps-region-to-file" . jj-color-ps-region-to-file))

(define-key menu-bar-jj-menu [item-109]
  '("color-ps-buffer-to-file" . jj-color-ps-buffer-to-file))

;;(define-key menu-bar-jj-menu [item-1010]
;;  '("dim-colors" . jj-dim-colors))
;;
;;(define-key menu-bar-jj-menu [item-1015]
;;  '("brighten-colors" . jj-brighten-colors))
;;
;;(define-key menu-bar-jj-menu [item-120]
;;  '("factor  ;-)" . jj-factor))

(define-key menu-bar-jj-menu [item-122]
  '("xdvi-at-pos" . jj-xdvi-at-pos))

(define-key menu-bar-jj-menu [item-130]
  '("tabify-makefile" . jj-tabify-makefile))

(define-key menu-bar-jj-menu [item-140]
  '("untabify-buffer" . jj-untabify-buffer))

;;(define-key menu-bar-jj-menu [item-150]
;;  '("my-open-curly-braces" . jj-my-open-curly-braces))

(define-key menu-bar-jj-menu [item-160]
  '("load-dotemacs" . jj-load-dotemacs))

(define-key menu-bar-jj-menu [item-170]
  '("kill-crtl-M" . jj-delete-crtl-M))

;; Last is top entry in menu:
(define-key menu-bar-jj-menu [item-180]
  '("delete-trailing-spaces" . jj-delete-trailing-spaces))



;;(provide 'menu)

;;; menu.el ends here
