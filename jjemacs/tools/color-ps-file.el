;;; color-ps-file.el --- file used by color-ps-file.sh

;;; Commentary:
;; Author: Joerg Arndt
;; online at http://www.jjj.de

;;; History:
;; version: 2005-July-24 (11:53)


;;; Code:


;;(setq window-system 'x)
;;(load-file "~/.emacs")


;;(iconify-frame)
;;(setq frame-title-format '("8-)"))

(unless (find-file-read-only (getenv "FILE")) (kill-emacs))  

;;(font-lock-mode nil) ; B/W
(if (= 1 (length (getenv "NOCOLOR")))
    (setq ps-print-color-p  nil)) ; B/W

(setq ps-paper-type 'a4)
;;(setq ps-default-bg '(0.95 0.95 0.95)) ; default is white

;;(setq ps-print-header nil)
;;(setq ps-header-offset (/ (* 72  1.0) 2.54)) ; 1.0 cm
(setq ps-header-offset (/ (* 72  0.25) 2.54)) ;
;;  "*Vertical space in points (1/72 inch) between the main text and the header."

;;(setq ps-print-footer t) (setq ps-footer-lines 1) ; default is no footer

(setq ps-header-font-family 'Helvetica)
;;(setq ps-font-family 'Helvetica)
(setq ps-font-family 'Courier) ; <--= fixed width
;;(setq ps-font-family 'Times)

;;(setq ps-paragraph-spacing 0.5)  ; default is 0
(setq ps-line-number t
      ps-line-number-start 1
      ps-line-number-step 1
      ps-line-number-font "Courier"
      ps-line-number-font-size 7 )

(setq ps-header-lines 1) ; 1, 2 or 3

(setq timestr (getenv "DATE"))
;;(setq timestr (concat "(" timestr ")"))
(setq dispname (getenv "DNAME"))
;;(setq dispname (concat "(" dispname ")"))
;;(setq ps-left-header (list (concat dispname timestr)))
(setq ps-left-header (list (concat "(" dispname "     " timestr ")")))

;;(setq ps-show-n-of-n t)

;;(setq timestr (shell-command-to-string "date +'%Y-%B-%d' | tr -d '\n' "))
;;(setq ps-right-header
;;      (list (concat  "(" timestr ")")))


(ps-print-buffer-with-faces (getenv "PSFILE"))
(kill-emacs)


;;(provide 'color-ps-file)

;;; color-ps-file.el ends here
