;;; funcs.el --- function definitions
;; (La)TeX definitions are in funcs-tex.el

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:08)
;;----------------------------------------------


;;; bound to [(shift insert)] in xwin.el:
(defun jjj-insert-primary ()
  "Insert mouse selection (bound to shift-insert)."
  (interactive)
  (progn
    (let ((primary (x-get-selection 'PRIMARY) ))
      (insert primary)
      ) ) )


(defun goto-column (n)
  "Goto column ARG, counting from column 0.  Argument N column number."
  (interactive "NGoto column: ")
  (move-to-column n) )

;;
(defun jjj-match-paren ()
  "Move to the parenthesis matching the one under the cursor."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1) t)
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1) t)))

;;
(defun jjj-insert-date ()
  "Insert date at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "%Y-%B-%d" (current-time)))  )

;;
(defun jjj-insert-oeis-sig ()
  "Insert name and date (in OEIS format) at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "_Joerg Arndt_, %b %d %Y" (current-time)))  )

;;
(defun jjj-insert-date-time ()
  "Insert date and time at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d (%H:%M)' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "%Y-%B-%d (%H:%M)" (current-time)))  )


;;
(defun jjj-insert-rand128 ()
  "Insert 128-bit random number (in hex) at point."
  (interactive)
  (shell-command
   "dd if=/dev/urandom count=1 2>/dev/null | md5sum -b | cut -d' ' -f1 | tr -d '\n' "
   (quote (4)) nil) )

;; note: may use delete-trailing-whitespace
(defun jj-delete-trailing-spaces ()
  "Delete trailing spaces (or tabs) in all lines."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1) (replace-regexp "[ \t]+$" "") ) ) )


;; delete all ^M (dos --> unix line endings)
(defun jj-delete-crtl-M ()
  "Delete all ^M (dos --> unix line endings)."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1)
;;      (replace-regexp "+" "")
      (replace-regexp "+$" "")
;;      (while (search-forward "" nil t) (replace-match "" nil t)) ;; not faster
      ) ) )


;; The variable `tab-width' controls the spacing of tab stops.
(defun jj-untabify-buffer ()
  "Untabify whole buffer."
  (interactive)
  (untabify (point-min) (point-max)) )

;;
(defun jj-tabify-makefile ()
  "Place tabs where needed in makefiles."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1) (replace-regexp "^        " "\t") ) ) )


;;
(defun jj-color-ps-buffer-to-file (F)
  "Save postscript file including fonts to specified file.  Argument F filename."
  ;; (ps-print-buffer-with-faces &optional FILENAME)
  (interactive "F file to save ps to: ")
  (progn (ps-print-buffer-with-faces F) ) )

;;
(defun jj-color-ps-region-to-file  (F)
  "Save postscript file including fonts to specified file.  Argument F filename."
  (interactive "F file to save ps to: ")
  ;; (ps-print-region-with-faces FROM TO &optional FILENAME)
  (progn (ps-print-region-with-faces (mark) (point) F) ) )


;;
(defun jj-ps-buffer-to-file (F)
  "Save postscript file to specified file.  Argument F filename."
  ;; (ps-print-buffer-with-faces &optional FILENAME)
  (interactive "F file to save ps to: ")
  (progn (ps-print-buffer F) ) )

;;
(defun jj-ps-region-to-file  (F)
  "Save postscript file to specified file.  Argument F filename."
  (interactive "F file to save ps to: ")
  ;; (ps-print-region-with-faces FROM TO &optional FILENAME)
  (progn (ps-print-region (mark) (point) F) ) )


;;
(defun jj-reopen-file ()
  "Reopen file in buffer."
  (interactive)
  (let ((p (point)))
    (progn
      (find-alternate-file buffer-file-name)
      (goto-char p)
      (recenter) ) ) )

(defun jj-reopen-file-read-only ()
  "Reopen file in buffer read-only."
  (interactive)
  (progn
    (jj-reopen-file)
    (toggle-read-only 1) ) )


(defun jj-reopen-file-too ()
  "Reopen file in buffer, jump to first line identical to current."
  (interactive)
    (progn
      (recenter)
      (beginning-of-line)
      (let ((p1 (point)))
        (end-of-line)
        (let ((p2 (point)))
          (copy-region-as-kill p1 p2)
          (find-alternate-file buffer-file-name)
          (goto-char 1)
          (nonincremental-search-forward (current-kill 0))
          (recenter)
          ) ) ) )

;;
(defun jj-load-dotemacs ()
  "Load ~/.emacs."
  (interactive)
  (load-file "~/.emacs") )

;;
(defun jjj-kill-ring-save-line ()
  "Add current line to `kill-ring'."
  (interactive)
  (progn
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
;;      (kill-ring-save beg (point))
      (kill-new (buffer-substring beg (point)))
      ) ) )


;;
(defun jjj-duplicate-line ()
  "Duplicate current line."
  (interactive)
  (progn
    (jjj-kill-ring-save-line) ; save line
    (save-excursion ; duplicate line
      (end-of-line)
      (insert "\n")
      (yank) )
    (let ( (n (jjj-get-col-number)) ) ; move to new line, goto same column
      (forward-line +1)
      (move-to-column n) )
    ) )

;;
(defun jjj-delete-line ()
  "Delete current line."
  (interactive)
  (progn (beginning-of-line) (kill-line 1) ) )

;;
(defun jj-nsplit-line (n)
  "Split line into pieces of length N."
  (interactive "nSplit into pieces of length n: ")
  (progn
    (if (< n 1) (error "n must be greater than zero"))
    (let ((beg) (end) (stp))
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (setq stp (point))
      (while (< beg end)
        (goto-column n)
        (insert "\n")
        (setq beg (+ n beg)) )
      (goto-char stp) ) ) )

;;
(defun jj-delnl-region ()
  "Delete newlines in region (make one long line)."
  (interactive)
  (save-excursion
    (let ((p0 (mark)) (p1 (point)))
      (goto-char p0)
      (end-of-line)
      (while (< (point) p1)
        (delete-char 1)
        (setq p1 (- p1 1))
        (end-of-line) ) ) ) )

;;
(defun jjj-get-line-number ()
  "Return line number of point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let
        (
         (opoint (point))
         (nline)
         )
      (goto-char (point-min))
      (setq nline (1+ (count-lines (point) opoint)))
      nline
      )
    )
  )

;;
(defun jjj-get-col-number ()
  "Return column number of point."
  (interactive)
  (save-excursion
    (let
        (
         (opoint (point))
         (ncol)
         )
      (beginning-of-line)
      (setq ncol (- opoint (point)))
      ncol
      )
    )
  )

;;
(defun jj-count-matches-region (r)
  "Count occurences of REGEXP in region."
 (interactive "s Enter regexp: ") ; elips.ps.gz p.335
  (progn
;;    (message "%d" (region-beginning))
;;    (message "%d" (region-end))
;;    (message "%S" r)
    (how-many r (region-beginning) (region-end))
    )
  )

;; from: http://www.emacswiki.org/emacs/FindingNonAsciiCharacters
;;(defun find-first-non-ascii-char ()
(defun ascii-check ()
  "Move cursor under the first non-ascii character in file."
  (interactive)
  (let (point)
    (save-excursion
      (goto-char (point-min)) ;; check whole file
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char)) 'ascii)
                    (progn
                      (message "Non-ascii now under cursor.")
                      (throw 'non-ascii (point)) )
                    )
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "OK (no non-ascii characters in file)."))))

(defun ascii-check-next ()
  "Move cursor under the first non-ascii character after point."
  (interactive)
  (let (point)
    (forward-char)  ;; after point
    (save-excursion
;;      (goto-char (point-min)) ;; check whole file
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char)) 'ascii)
                    (progn
                      (message "Non-ascii now under cursor.")
                      (throw 'non-ascii (point)) )
                    )
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "OK (no non-ascii characters after point)."))))




(defun jjj-insert-pari-func ()
  "Insert stub for function in pari/gp."
  (interactive)
  (progn
    (insert
     "()=\n"
     "{\n"
;;     "    local();\n"
     "    my();\n"
     "    return();\n"
     "} /* ----- */\n")
    (previous-line 5)
    (beginning-of-line) ) )


;;
(defun jjj-compile ()
  "Run make (target=`make-target' or Etarget) in directory `makefile-dir' (or ./).
If the variable `make-args' is set then it is passed to make."
  (interactive)
  (let (;(d (jjj-get-TeX-basedir))
        (m (if (boundp 'make-target) make-target "Etarget"))
        (d (if (boundp 'makefile-dir) makefile-dir "./"))
        (a (if (boundp 'make-args) make-args ""))
        )
    (compile (concat "cd " d " && make " m " " a) )
    )
  )
;; for key-binding:
(defun jjj-save-and-compile ()
  "Call jjj-compile."
  (interactive)
  (progn
    (unless (eq (buffer-file-name) nil)
        (progn
;;          (message "REG")
          (recenter)
          (save-buffer)
          )
        )
    (jjj-compile) ) )

;;
(defun jjj-compile2 ()
  "Run make (target=`make-target2' or Etarget2) in directory `makefile-dir' (or ./).
If the variable `make-args' is set then it is passed to make."
  (interactive)
  (let (
        (m (if (boundp 'make-target2) make-target2 "Etarget2"))
        (d (if (boundp 'makefile-dir) makefile-dir "./"))
        (a (if (boundp 'make-args) make-args ""))
        )
    (compile (concat "cd " d " && make " m " " a) )
    )
  )
;; for key-binding:
(defun jjj-save-and-compile2 ()
  "Call jjj-compile2."
  (interactive)
  (progn
    (unless (eq (buffer-file-name) nil)
        (progn
;;          (message "REG")
          (recenter)
          (save-buffer)
          )
        )
    (jjj-compile2) ) )


;;
(defun cur-file ()
  "Return the filename (without directory) of the current buffer"
  (interactive)
  (file-name-nondirectory (buffer-file-name (current-buffer)))
  )

;;
(defun cur-directory ()
  "Return the directory of the current buffer"
  (interactive)
  (file-name-directory (buffer-file-name (current-buffer)))
  )

;;
(defun jjj-base-name (file-name)
  (car (split-string file-name "\\."))
;;  (file-name-sans-extension buffer-file-name);; incl. dir
  )
;; fixme: transforms aaa.bb.txt into aaa (and not aaa.bbb)

;;
(defun jjj-insert-path-name ()
  "Insert buffer-file-name (with path) at point."
  (interactive)
  (insert buffer-file-name) )

;;
(defun jjj-insert-file-name ()
  "Insert buffer-file-name (without path) at point."
  (interactive)
  (insert (cur-file) ) )

;;
(defun jjj-insert-base-name ()
  "Insert buffer-file-name (without path, without extension) at point."
  (interactive)
  (insert (jjj-base-name (cur-file)) ) )


;;(defun jj-cusk ()
;;  "Update copyright, save and kill buffer."
;;  (interactive)
;;  (progn
;;    (copyright-update nil t)
;;    (save-buffer)
;;    (kill-this-buffer)
;;    ) )



;;(provide 'funcs)


;;; funcs.el ends here
