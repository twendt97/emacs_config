;;; comments.el --- comment functions

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:08)
;;----------------------------------------------

;; Functions defined here are bound to keystrokes in "keys.el".

;; The functions `jjj-delete-*-string' are used in `jjj-delete-*-comment'
;; and could be improved in order to skip lines without comments.

(defun jjj-delete-string (s)
  "Delete string S."  (interactive)
  (let ((n (length s)))
    (while (> n 0)
      (progn
        (delete-char 1)
        (setq n (- n 1)) ) ) )
  )

(defun jjj-backward-delete-string (s)
  "Backward delete string S."  (interactive)
  (let ((n (length s)))
    (while (> n 0)
      (progn
        (backward-delete-char 1)
        (setq n (- n 1)) ) ) )
  )


;;; LINE COMMENTS:
(defun jjj-insert-comment (s)
  "Insert S at beginning of line to comment line out."  (interactive)
  (progn
    (beginning-of-line)
    (progn
      (insert s)
      (beginning-of-line)
      (delete-horizontal-space)
      (beginning-of-line 2) )
    (recenter) )
  )

(defun jjj-delete-comment (s)
  "Delete S at beginning of line to uncomment line."  (interactive)
  (progn
    (beginning-of-line)
    (progn
      (delete-horizontal-space)
      (jjj-delete-string s)
      (beginning-of-line 2) )
    (recenter) )
  )

(defun jjj-indent-with-comment (s)
  "Remove comment, indent and comment out again.
Argument S comment-string."
  (progn
    (jjj-delete-comment s)
    (beginning-of-line 0)
    (indent-for-tab-command)
    (jjj-insert-comment s)
    )
  )


;;; BLOCK COMMENTS:
(defun jjj-insert-block-comment (sb se)
  "Put block comment around current line.
Argument SB comment-begin-string.
Argument SE comment-end-string."
  (interactive)  (progn
                   (beginning-of-line)
                   (insert sb)
                   (end-of-line)
                   (delete-horizontal-space)
                   (insert se)
                   (beginning-of-line 2)
                   (recenter)
                   ) )

(defun jjj-delete-block-comment (sb se)
  "Remove block comment around current line.
Argument SB comment-begin-string.
Argument SE comment-end-string."
  (interactive)  (progn
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (jjj-delete-string sb)
                   (end-of-line)
                   (delete-horizontal-space)
                   (jjj-backward-delete-string se)
                   (beginning-of-line 2)
                   (recenter)
                   ) )


;; The following lines used to trigger the message
;; Loading `comments.el': old-style backquotes detected!
;; Solution:  (thanks go to Andreas Schwab), change
;;   [(control ,)]  -->  [(control \,)]
;;   [(control c) (,)]  -->   [(control c) (\,)]
(global-set-key [(control \,)]     '(lambda () (interactive) (jjj-insert-comment "#")) )
(global-set-key [(control c) (\,)] '(lambda () (interactive) (jjj-insert-comment "#")) )
(global-set-key [(control <)]     '(lambda () (interactive) (jjj-delete-comment "#")) )
(global-set-key [(control c) (<)] '(lambda () (interactive) (jjj-delete-comment "#")) )


(global-set-key [(control .)]     '(lambda () (interactive) (jjj-insert-block-comment "/*" "*/")) )
(global-set-key [(control c) (.)] '(lambda () (interactive) (jjj-insert-block-comment "/*" "*/")) )
(global-set-key [(control >)]     '(lambda () (interactive) (jjj-delete-block-comment "/*" "*/")) )
(global-set-key [(control c) (>)] '(lambda () (interactive) (jjj-delete-block-comment "/*" "*/")) )



;;(provide 'comments)

;;; comments.el ends here
