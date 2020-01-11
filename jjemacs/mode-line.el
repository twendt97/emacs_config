;;; mode-line.el --- customize mode line

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:09)
;;----------------------------------------------


;; Template for displaying mode line for current buffer.
(setq-default mode-line-format
      '(""
        mode-line-modified
        "[%b]"
        global-mode-string
        "%[("
        mode-name
        mode-line-process
        minor-mode-alist
        "%n" ")%]--"
        (line-number-mode "L%l--")
        (column-number-mode "C%c--")
        (-3 . "%p")  ;; position
        " -- "
;;        user-login-name "@" system-name  ;; you@host.domain.org
        user-login-name "@" hostname  ;;  you@host
        ":"
;;        "%f"  ;; print file with full path
        (:eval buffer-file-truename)  ;; print file with abbreviated path
        " %-"
        ) )



;;(provide 'mode-line)

;;; mode-line.el ends here
