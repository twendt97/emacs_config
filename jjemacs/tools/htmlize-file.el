;;; htmlize-file.el --- file used by htmlize-file.sh

;;; Commentary:
;; Author: Joerg Arndt
;; online at http://www.jjj.de
;; based on http://www.dd.chalmers.se/~bojohan/emacs/lisp/my-htmlize.el

;;; History:
;; version: 2005-February-19 (11:27)


;;; Code:

;;(iconify-frame)
;;(setq frame-title-format '("8-)"))

;;(load-file "~/.emacs")
(require 'htmlize)

(unless (find-file-read-only (getenv "FILE")) (kill-emacs))  

;;(find-file (pop command-line-args-left))
(font-lock-fontify-buffer)
;;(with-current-buffer (htmlize-buffer) (princ (buffer-string)))
(with-current-buffer (htmlize-buffer) (write-file "tmp-htmlize-out-tmp.html"))
;;(htmlize-file (getenv "FILE"))

(kill-emacs)

;;; htmlize-file.el ends here
