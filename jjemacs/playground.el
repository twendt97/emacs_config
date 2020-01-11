;;; playground.el ---  playground

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:07)
;;----------------------------------------------


(defun jj-factor (n)
  "Print factors of N."
  (interactive "n Enter number to factor: ")
  (progn
    (message "%d factors into:" n)
    (if (> n 1)
        (progn
          (while (= (mod n 2) 0)        ; remove factor 2
            (progn
              (setq n (/ n 2))
              (message "  %d" 2)))
          (if (> n 1)
              (progn
                (let ((f 3))
                  (while (<= f n)
                    (progn
                      (while (= (mod n f) 0) ; remove odd factors
                        (progn
                          (setq n (/ n f))
                          (message "  %d" f)))
                      (setq f (+ 2 f)))))))))
    (switch-to-buffer "*Messages*")
    (goto-char (point-max))
    (recenter)))

;;(defun fact (n) "factorial(n)" (if (< n 2) 1 (* n (fact (- n 1)))))

(defun jj-sort-chars ()
  "Sort chars of current line (needs external command char-sort)."
  (interactive)
  (progn
    (save-excursion
      (let ((p1) (p2))
        (beginning-of-line)  (setq p1 (point))
        (end-of-line)  (setq p2 (point))
        (shell-command-on-region p1 p2 "char-sort" t nil nil)
        ) ) ) )


(defun jj-reverse-word ()
  "Reverse order of (each line of) chars between mark and point (needs external command rev)."
  (interactive)
  (progn
    (save-excursion
      (let ((p1 (mark)) (p2 (point)))
        (shell-command-on-region p1 p2 "rev" t nil nil)
        (goto-char p2)
        (delete-char 1)
        ) ) ) )

(defun jj-reverse-line ()
  "Reverse (order of chars in) current line (needs external command rev)."
  (interactive)
  (progn
    (save-excursion
      (beginning-of-line)
      (let ((p1 (point)) (p2 0))
        (end-of-line)
        (setq p2 (point))
;;          (shell-command-on-region p1 p2 "rev" nil nil nil) ; output as message
        (shell-command-on-region p1 p2 "rev" t nil nil)
        )
      (end-of-line)
      (delete-char 1)
      (beginning-of-line)
    ) ) )

(defun jj-search-region ()
  "Search strings identical to string in region."
  (interactive)
    (progn
      (copy-region-as-kill (region-beginning) (region-end))
      (goto-char 1)
      (nonincremental-search-forward (current-kill 0))
      ) )

;;
(defun jj-indent-next-c-func ()
  "Goto next '{' at beginning of line and indent C/C++-code until corresponding '}'."
  (interactive)
  (progn
    (search-forward-regexp "^\{")
    (backward-char 1)
    (c-indent-exp)
    (forward-char 1) ) )

;;
(defun jj-indent-next-el-func ()
  "Goto next '(' at beginning of line and indent elisp-code until corresponding ')'."
  (interactive)
  (progn
    (search-forward-regexp "^\(")
    (backward-char 1)
    (indent-sexp)
    (forward-char 1) ) )




;;
;;(defun jjj-byte-compile-my-files ()
;;  "byte compile  ~/.jjemacs/*.el"
;;  (interactive)
;;  (progn
;;    (byte-compile-file "~/.jjemacs/main.el")
;;    (byte-compile-file "~/.jjemacs/settings.el")
;;    (byte-compile-file "~/.jjemacs/keys.el")
;;    (byte-compile-file "~/.jjemacs/funcs.el")
;;    (byte-compile-file "~/.jjemacs/comments.el")
;;    (byte-compile-file "~/.jjemacs/hooks.el")
;;    (byte-compile-file "~/.jjemacs/modes.el")
;;    (byte-compile-file "~/.jjemacs/xwin.el")
;;    (byte-compile-file "~/.jjemacs/my-font-lock.el")
;;    (byte-compile-file "~/.jjemacs/my-hilit.el")
;;    ) )



;; ?
;;(defun jj-find-file-if-exists (str) "if file \"str\" exists, open it."
;;  (interactive "S Find file \(if exists\):")
;;  (if (file-exists-p str)
;;      (find-file str)
;;    (print (cons str "does not exist.")))
;;  )

;; TEST:
;;(defun jj-test () "Test, might erase your harddisk."
;;  (interactive)
;;  (jj-my-open-curly-braces) )
;;
;;(defun jj-test ()  "Test."  (interactive)  (message "%s" (read-event "E")))
;;(defun jj-test ()  "Test."  (interactive)  (message "%s" (read-key-sequence "S")))
;;(defun jj-test ()  "Test."  (interactive)  (message "%s" (read-key-sequence-vector "V")))

;;
;;(defun jj-mouse-set-point (click)
;;  (interactive "e")
;;;;  (cm-mouse-set-point click)
;;  (if (boundp 'mouse-init-event) nil (setq mouse-init-event t)))


;;(defun jj-search-marked-text ()
;;  "Search for text identical to marked."
;;  (interactive)
;;    (progn
;;      (copy-region-as-kill (mark) (point))
;;;;      (goto-char 1)
;;      (nonincremental-search-forward (current-kill 0))
;;      ) )

;;
(defun jj-my-open-curly-braces ()
  "Put open curly braces at line ends to start of next line."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1)
      (replace-regexp "\\(.+\\){$" "\\1\n{") ) ) )



(provide 'playground)

;;; playground.el ends here
