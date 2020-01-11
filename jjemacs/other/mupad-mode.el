;;; bij - 1997/98
;;; mupad-mode.el  -  Major mode for editing mupad source in emacs.

;;; Particulary based on the pascal-mode of Emacs.

;;; 
;;; Juergen Billing -- bij@mupad.de
;;;

;; 

;; Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; USAGE
;;; =====

;;;
;;;
;;;

;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; 
;;; 

;;; Code:

(defconst mupad-mode-version "1.5"
  "Version of `mupad-mode.el'.")

(setq isXEmacs (string-match "XEmacs" emacs-version))

(defvar mupad-mode-map ()
  "Keymap used in Mupad mode.")
(if mupad-mode-map
    ()
  (setq mupad-mode-map (make-sparse-keymap))
;  (define-key mupad-mode-map ";"        'electric-mupad-semi-or-dot)
;  (define-key mupad-mode-map ":"        'electric-mupad-colon)
;  (define-key mupad-mode-map "#"        'electric-mupad-hash)
  (define-key mupad-mode-map "\r"       'electric-mupad-terminate-line)
  (define-key mupad-mode-map "\""       'electric-mupad-open-quote)
  (define-key mupad-mode-map "("        'electric-mupad-open-brace)
  (define-key mupad-mode-map "["        'electric-mupad-open-brace)
  (define-key mupad-mode-map "{"        'electric-mupad-open-brace)
  (define-key mupad-mode-map "\M-\r"    'newline)
  (define-key mupad-mode-map "\t"       'electric-mupad-tab)
  ;(define-key mupad-mode-map "\M-?"     'mupad-show-completions)
  (define-key mupad-mode-map "\177"     'backward-delete-char-untabify)
;  (define-key mupad-mode-map "\M-\C-h"  'mupad-mark-defun)
;  (define-key mupad-mode-map "\C-c\C-b" 'mupad-insert-block)
  (define-key mupad-mode-map "\M-*"     'mupad-star-comment)
   ;(define-key mupad-mode-map "\S-\t"    'tab)
   (define-key mupad-mode-map "\C-c\C-c" 'mupad-comment-region)
   (define-key mupad-mode-map "\C-c\C-e" 'mupad-close-statement)
   (define-key mupad-mode-map "\C-c\C-u" 'uncomment-region)
   (define-key mupad-mode-map "\C-c\C-x" 'mupad-exinclude-area)
   (define-key mupad-mode-map "\C-c\C-x" 'mupad-exinclude-area)
   (define-key mupad-mode-map [?\C->]     'mupad-indent-region)
   (define-key mupad-mode-map [?\C-<]     'mupad-unindent-region)
   (define-key mupad-mode-map "\M-p"     'mupad-fun-to-proc)
   (define-key mupad-mode-map "\e\t"     'mupad-complete-symbol)
   ;(define-key mupad-mode-map "\M-\t"    'mupad-complete-word)
   )

;(defvar mupad-keywords
;   '("and" "begin" "break" "case" "div" "do" 
;     "downto" "E" "elif" "else" "end" "end_case" "end_for"  "end_if" 
;     "end_proc" "end_repeat" "end_while" "FAIL"
;     "FALSE" "for" "from" "I" "if" "in" "intersect" 
;     "local" "minus" "mod" "name" "next" "NIL" "not" "of"
;     "option" "or" "otherwise" 
;     "proc" "quit" "repeat" "step" "then" "to" "TRUE"
;     "union" "until" "UNKNOWN" "while ")
;   "MuPAD keywords to hilight while editing")

(message "loading mupad-cpl...")
(require 'mupad-cpl)
(message "loading mupad-cpl...done")

;;;###autoload
(defun mupad-create-completions ()
  ""
  (setq mupad-completion-list (make-vector 1117 0))
  (mapcar (lambda (x) (intern x mupad-completion-list)) mupad-all-completions)
  (setq mupad-libraries-completion-list (make-vector 1117 0))
  (mapcar (lambda (x)
	    (let ((dom (cdr (assoc x mupad-libraries-completion-alist))))
	      (if (not (eq dom nil))
		  (mapcar (lambda (m)
			    (intern (concat x "::" m) mupad-libraries-completion-list))
			  dom))))
	  mupad-libraries-list)
  )

 ;;;
 ;;; Regular expressions used to calculate indent, etc.
 ;;;
(defconst mupad-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9]*\\(::[a-zA-Z_][a-zA-Z_0-9]*\\)?\\>")
(defconst mupad-beg-block-re   "\\<\\(proc\\|case\\|repeat\\|while\\|if\\|for\\|domain\\|category\\|axiom\\)\\>")
(defconst mupad-end-block-re   "\\<\\(end\\(_\\(proc\\|if\\|for\\|case\\|while\\|domain\\|category\\|axiom\\)\\)?\\|until\\)\\>")
(defconst mupad-sub-block-re   "\\<\\(el\\(se\\|if\\)\\|begin\\)\\>")
(defconst mupad-noindent-re    "\\<\\(el\\(se\\|if\\)\\|begin\\|end\\(_\\(proc\\|if\\|for\\|case\\|while\\|domain\\|category\\|axiom\\)\\)?\\|until\\)\\>")
(defconst mupad-casesub-re     "\\<\\(of\\|otherwise\\)\\>")

(defconst mupad-autoindent-lines-re
  "\\<\\(end\\(_\\(proc\\|if\\|for\\|case\\|while\\|domain\\|category\\|axiom\\)\\)?\\|begin\\|of\\|do\\|otherwise\\|el\\(se\\|if\\)\\)\\>")

 ;;; Strings used to mark beginning and end of excluded text
(defconst mupad-exclude-str-start "/*----\\/----- EXCLUDED -----\\/-----")
(defconst mupad-exclude-str-end   " -----/\\----- EXCLUDED -----/\\----*/")

(defvar mupad-mode-syntax-table nil
  "Syntax table in use in Mupad-mode buffers.")

(defvar mupad-no-closed-brace nil
  "Turn off self-closed braces.")

(if mupad-mode-syntax-table
    ()
  (setq mupad-mode-syntax-table (make-syntax-table))
 ;  (set-syntax-table mupad-mode-syntax-table)
 ;	(while (< char ? )
 ;	  (modify-syntax-entry char ".")
 ;	  (setq char (1+ char)))
  (modify-syntax-entry ?\\ "\\"  mupad-mode-syntax-table)
  (modify-syntax-entry ?( "()"  mupad-mode-syntax-table)  
  (modify-syntax-entry ?) ")("  mupad-mode-syntax-table)
  (modify-syntax-entry ?[ "(]"  mupad-mode-syntax-table)  
  (modify-syntax-entry ?] ")["  mupad-mode-syntax-table)
  (modify-syntax-entry ?{ "(}"  mupad-mode-syntax-table)  
  (modify-syntax-entry ?} "){"  mupad-mode-syntax-table)
  (modify-syntax-entry ?# "$#"   mupad-mode-syntax-table)
  (modify-syntax-entry ?\~ "."  mupad-mode-syntax-table)
  (modify-syntax-entry ?+ "."   mupad-mode-syntax-table)
  ;(modify-syntax-entry ?\" "."  mupad-mode-syntax-table)
  (modify-syntax-entry ?- "."    mupad-mode-syntax-table)
  ;(modify-syntax-entry ?/  ". 1456" mupad-mode-syntax-table) ; XEmacs?
  (modify-syntax-entry ?/  ". 124b" mupad-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"   mupad-mode-syntax-table)

  (if isXEmacs
      (progn
	(modify-syntax-entry ?\n ">b"  mupad-mode-syntax-table)
	;; Give CR the same syntax as newline, for selective-display
	(modify-syntax-entry ?\^m ">b" mupad-mode-syntax-table)
	)
    (modify-syntax-entry ?\n "> b"  mupad-mode-syntax-table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b" mupad-mode-syntax-table)
    )
  (modify-syntax-entry ?= "."    mupad-mode-syntax-table)
  (modify-syntax-entry ?% "."    mupad-mode-syntax-table)
  (modify-syntax-entry ?< "."    mupad-mode-syntax-table)
  (modify-syntax-entry ?> "."    mupad-mode-syntax-table)
  (modify-syntax-entry ?$ "."    mupad-mode-syntax-table)
  (modify-syntax-entry ?| "."    mupad-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    mupad-mode-syntax-table)
 ;  (modify-syntax-entry ?% "\""  mupad-mode-syntax-table)
  (set-syntax-table mupad-mode-syntax-table)
  )

(defvar mupad-indent-level 2
   "*Indentation of Mupad statements with respect to containing block.")

(defvar mupad-case-indent 2
   "*Indentation for case statements.")

(defvar mupad-auto-newline t
   "*Non-nil means automatically newline after simcolons and the punctation mark
 after an end.")

(defvar mupad-tab-always-indent t
   "*Non-nil means TAB in Mupad mode should always reindent the current line,
 regardless of where in the line point is when the TAB command is used.")

(defvar mupad-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
 Elements can be of type: 'paramlist', 'declaration' or 'case', which will
 do auto lineup in parameterlist, declarations or case-statements
 respectively. The word 'all' will do all lineups. '(case paramlist) for
 instance will do lineup in case-statements and parameterlist, while '(all)
 will do all lineups.")

(defvar mupad-type-keywords
  '("DOM_INT" "DOM_RAT" "DOM_FLOAT" "DOM_COMPLEX" "DOM_STRING" "DOM_PROC"
    "DOM_EXEC" "DOM_EXPR")
  "*Keywords for types used when completing a word in a declaration or parmlist.
 \(eg. integer, real, char.)  The types defined within the Mupad program
 will be completed runtime, and should not be added to this list.")

(defvar mupad-start-keywords
  '("begin" "end_proc" "fun" "proc" "func" "until" "while" "repeat")
  "*Keywords to complete when standing at the first word of a statement.
 \(eg. begin, repeat, until, readln.)
 The procedures and variables defined within the Mupad program
 will be completed runtime and should not be added to this list.")

(defvar mupad-separator-keywords
  '("from" "to" "else" "of" "otherwise" "do" "then")
  "*Keywords to complete when NOT standing at the first word of a statement.
 \(eg. downto, else, mod, then.) 
 Variables and function names defined within the
 Mupad program are completed runtime and should not be added to this list.")

(defvar mupad-end-keywords
  '("end_proc" "end_if" "end_case" "end_while" "end_for")
  "*Keywords to complete when standing at the first word of a statement.
 \(eg. begin, repeat, until, readln.)
 The procedures and variables defined within the Mupad program
 will be completed runtime and should not be added to this list.")

 ;;;
 ;;;  Macros
 ;;;

(defsubst mupad-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst mupad-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defsubst mupad-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (mupad-get-beg-of-line) (point)))))

(defun mupad-ic ()
  ""
  (interactive)
  (message (if (mupad-within-comment)
	       (concat "in comment")
	     (concat "not in comment"))
	   )
  )

;;; bij - neu
(defun mupad-within-comment ()
  "..."
  (save-excursion
    (setq ende (point))
    (setq start (if (search-backward-regexp "/\\*" (point-min) t)
		    (point)
		  (point-min)))
    (nth 4 (parse-partial-sexp start ende))
    )
	
    ;(and (mupad-within-exclude)
    ;     (= 0 (nth 0 (parse-partial-sexp (mupad-get-beg-of-line) (point))))
    ;	  ())
;     (progn (setq here (point))
;	    (setq anz 0)
;	    (beginning-of-line)
;	    (while (re-search-forward "#" here t)
;	      (if (not (mupad-within-string))
;		  (setq anz (- 1 anz))))
;	    (= 0 anz)
;	    )
  )

;;; bij - neu
(defsubst mupad-within-exclude ()
  "..."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward mupad-exclude-str-start nil t)
			   (point)))
	(setq end (progn (search-forward mupad-exclude-str-end nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (or (equal start (point)) (<= end (point))))))

;;; bij - neu
(defun mupad-exinclude-area ()
  "Call mupad-comment-area or mupad-uncomment-area
append of context."
  (interactive)
  (save-excursion
    (setq here (point))
    (if (not (mupad-within-exclude))
	(mupad-uncomment-area)
      (progn
	(setq beg (region-beginning))
	(setq geb (region-end))
	(mupad-comment-area beg geb)
	)
      )))


;;; bij -neu
(defun mupad-test-function (&optional beg geb)
  "Test-Funktion"
  (interactive)
  (goto-char (region-beginning))
  (message "region begin")
  (sit-for 2)
  (goto-char (region-end))
  (message "region end")
  (sit-for 2)
  )


;;;###autoload
(defun mupad-mode ()
  "Major mode for editing Mupad code. \\<mupad-mode-map>
!!! This version is still under development. !!!

The main work is to indent code correctly while editing
and to colour the code.

\\[mupad-comment-region] will place '//' at beginning of each line in region
\\[uncomment-region] will  '//' at beginning of each line in region

Other useful functions are:

\\[mupad-star-comment]\t- insert /*  */
\\[mupad-indent-region]\t- indent region without influence the indent structure
\\[mupad-unindent-region]\t- reverse this
\\[electric-mupad-tab]\t- indent 
\\[electric-mupad-terminate-line]\t- insert newline with automatically indentation
M-<RET>\t- insert newline without indentation
C-<TAB>\t- insert standard tab without indentation

Variables controlling indentation/edit style:

 mupad-indent-level      (default 2)
    Indentation of Mupad statements with respect to containing block.

Turning on Mupad mode calls the value of the variable mupad-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mupad-mode-map)
  (setq major-mode 'mupad-mode)
  (setq mode-name "Mupad")
  (set-syntax-table mupad-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mupad-indent-line)
  (setq comment-indent-function 'mupad-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
;bij - comments
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "^\\(\\(//\\)+ *\\)")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  ;; Emacs - font-lock now takes care of this
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(mupad-font-lock-keywords
	  nil nil ((?_ . "w") (?~ . "w")) beginning-of-defun
	  (font-lock-comment-start-regexp . "/[*/]")
	  (font-lock-mark-block-function . mark-defun)))
  (font-lock-mode 1)
  (run-hooks 'mupad-mode-hook)
  (make-local-variable mupad-font-lock-comment-face)
  (make-local-variable mupad-font-lock-string-face)
  (make-local-variable mupad-font-lock-keyword-face)
  (make-local-variable mupad-font-lock-options-face)
  (make-local-variable mupad-font-lock-function-name-face)
  (make-local-variable mupad-font-lock-domain-face)
  ;(make-local-variable mupad-font-lock-variable-name-face)
  (make-local-variable mupad-font-lock-type-face)
  (make-local-variable mupad-font-lock-info-face)
  (make-local-variable 'default)
  (setq indent-tabs-mode nil)
  ;; initialize competions
  (mupad-create-completions)
  )

;(defun mupad-comment-indent ()
;  (if (or (looking-at "//") (looking-at "#("))
;      (current-column)
;    (skip-chars-backward " \t")
;    (max (if (bolp) 0 (1+ (current-column)))
;	 comment-column)))



;;;
;;;  Electric functions
;;;
(defun electric-mupad-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (delete-horizontal-space)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at mupad-autoindent-lines-re)
	(mupad-indent-line)))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (if (mupad-within-string)
      (insert "\\"))
  (newline)
  ;; Indent next line
  (mupad-indent-line)
  )

(defun electric-mupad-semi-or-dot ()
  "Insert `;' character and reindent the line."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (beginning-of-line)
    (mupad-indent-line))
  (if mupad-auto-newline
      (electric-mupad-terminate-line)))

(defun electric-mupad-open-brace ()
  "Insert closed brace."
  (interactive)
  (if mupad-no-closed-brace
      (insert last-command-char)
    (insert (setq br (char-to-string last-command-char)))
    (save-excursion
      (if (not (or (mupad-within-string)
		   (mupad-within-comment)
		   (string= (char-to-string (char-syntax (following-char))) "w")))
	  (insert (cond ((string= br "(") ")")
			((string= br "[") "]")
			((string= br "{") "}"))))
      )
    )
  )

(defvar mupad-string-opened 0)

(defun electric-mupad-open-quote ()
  "Insert closed quote."
  (interactive)
  (if (mupad-within-string)
      (if (= mupad-string-opened 0)
	  (insert "\\\"")
	(insert "\"")
	(setq mupad-string-opened 0))
    (insert "\"")
    (if (not (string= (char-to-string (char-syntax (following-char))) "w"))
	(save-excursion
	  (insert "\""))
      (setq mupad-string-opened 1))
    )
  )

(defun electric-mupad-tab ()
  "Function called when TAB is pressed in Mupad mode."
  (interactive)
  ;; Do nothing if within a string or in a comment.
  (cond ;((mupad-within-string)
	; (insert "\\t"))
	((mupad-within-comment)
	  ;(and (not (bolp))
	  ;     (save-excursion (beginning-of-line) (eq (following-char) ?#)))
	 (message "no indent while in comment"))
	(() ; only-comment-line
	 ())
        ;; If mupad-tab-always-indent, indent the beginning of the line.
	(t ; else
	 (if mupad-tab-always-indent
	     (save-excursion
	       (end-of-line)
	       (delete-horizontal-space)
	       (beginning-of-line)
	       (mupad-indent-line))
	   (insert "\t"))
	 (end-of-line)
	 ))
  )



;;;
;;; Interactive functions
;;;
;(defun mupad-insert-block ()
;  "Insert Mupad begin ... end; block in the code with right indentation."
;  (interactive)
;  (mupad-indent-line)
;  (insert "begin")
;  (electric-mupad-terminate-line)
;  (save-excursion
;    (electric-mupad-terminate-line)
;    (insert "end_proc:")
;    (beginning-of-line)
;    (mupad-indent-line)))

(defun mupad-star-comment ()
  "Insert Mupad star comment at point."
  (interactive)
  ;(mupad-indent-line)
  (if (not (mupad-within-comment))
      (progn (if (or (bolp)
		     (string= (char-to-string
				(char-syntax (preceding-char))) " "))
		 (insert "/* ")
	       (insert " /* "))
	     ;(electric-mupad-terminate-line)
	     (save-excursion
             ;  (electric-mupad-terminate-line)
             ;  (delete-horizontal-space)
	       (insert " */")))
    (message "Stay always in commented-area or parenhtesis mismatch.")
    ))

(defun mupad-mark-defun ()
  "Mark the current mupad function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (mupad-end-of-defun)
  (push-mark (point))
  (mupad-beg-of-defun)
  (if (fboundp 'zmacs-activate-region)
      (zmacs-activate-region)))


;;;
;;; Indentation
;;;
(defconst mupad-indent-alist
  '((block . (+ ind mupad-indent-level))
    (caseblock . (+ ind mupad-indent-level mupad-indent-level))
    (casesub . (+ ind mupad-indent-level))
    (cpp . 0)
    (declaration . (+ ind mupad-indent-level))
    (comment . 0)
    (defun . ind) (contexp . ind)
    (unknown . 0) (string . 0)))

(defun mupad-indent-line ()
  "Indent current line as a Mupad statement."
  (let* ((indent-str (mupad-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (delete-horizontal-space)
    ;; Some things should not be indented
    (if (eq type 'cpp)
	()
      ;; Other things should have no extra indent
      ;(message (concat type))
      ;(sit-for 1)
      (cond ((looking-at mupad-noindent-re)
	     (indent-to ind))
	    ((looking-at mupad-casesub-re)
	     (indent-to (+ ind mupad-indent-level)))
	    ;; But most lines are treated this way:
	    (t (indent-to (eval (cdr (assoc type mupad-indent-alist)))))
	    )))
  )

(defun mupad-indent-buffer ()
  "Indent each line of buffer."
  (interactive)
  ;(indent-region)
 )

(defun mupad-calculate-indent ()
  "Calculate the indent of the current Mupad line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((oldpos (point))
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (curcol (current-column)) (exs 0) (exsx 0) (exsb 0) (exse 0)
	   (complete (looking-at (concat "[ \t]*" mupad-end-block-re))) ; endblock
	   ;(ass nil)
	   (ass (looking-at (concat mupad-symbol-re "[ \t]*:="))) (assex nil) ; assign
	   (lastmatch "") (lastmatchp 0)
	   (elsed (looking-at (concat "[ \t]*" mupad-sub-block-re))) ; subblock
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (save-excursion
			    (goto-char (scan-lists (point) -1 1))
					;(backward-sexp 1)
			    (setq par (1+ (current-column)) exsx (point)))
			  (setq exs (scan-lists (point) -1 (car state))) ; exit subsexpression
;			  (if (> (point) (save-excursion
;						(search-backward-regexp mupad-end-block-re exs t)
;						(point))))
			  (save-excursion 
			    (search-backward-regexp mupad-beg-block-re exsx t)
			    (setq exsb (point)))
			  (save-excursion 
			    (search-backward-regexp mupad-end-block-re exsx t)
			    (setq exse (point)))
			  (if (or (= (point) exsb) (and (not (= (point) exse)) (> exse exsb)))
			      (goto-char exs))
			  )
			 ((save-excursion (beginning-of-line)
					  (or (eq (following-char) ?#)
					      (and (eq (following-char) ?/)
						   (or (eq (save-excursion
							     (forward-char)
							     (following-char)) ?/)
						       (eq (save-excursion
							     (forward-char)
							     (following-char)) ?/))
						   )))
			  (throw 'nesting 'cpp)
			  )
			 )
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--ignore comments
			    (mupad-within-comment)
			    (forward-comment -1)
			    )
		           (;--Nest block outwards
			    (looking-at mupad-beg-block-re)
			    (setq lastmatch (buffer-substring (match-beginning 0) (match-end 0))
				  lastmatchp (match-beginning 0))
			    (if (= nest 0)
				(progn (setq curcol (current-column))
				       (cond ((looking-at "\\<case\\>") (throw 'nesting 'caseblock))
					     ( t (throw 'nesting 'block))))
			      (setq nest (1- nest) ;assex t)
				    )
			      ))
			   (;--Nest block inwards
			    (looking-at mupad-end-block-re)
			    (setq lastmatch (buffer-substring (match-beginning 0) (match-end 0))
				  lastmatchp (match-beginning 0))
			    (setq complete t ;assex t
				  nest (1+ nest))
			    ; if block end found
			    )
			   (;--else, elif, of, otherwise
			    (and (not complete)
				 (looking-at mupad-sub-block-re))
			    (setq lastmatch (buffer-substring (match-beginning 0) (match-end 0))
				  lastmatchp (match-beginning 0))
			    (setq curcol (current-column))
			    (if (or (looking-at "\\<of\\>") (looking-at "\\<otherwise\\>"))
				(throw 'nesting 'casesub)
			      (throw 'nesting 'block)))
			   (;--Found assignment
			    (and (not assex) (looking-at (concat mupad-symbol-re "[ \t]*:=")))
			    (setq lastmatch (buffer-substring (match-beginning 0) (match-end 0))
				  lastmatchp (match-beginning 0))
			    (if (bolp) ; assign at left margin
				(progn (setq lastmatch "bobp" lastmatchp (match-beginning 0))
				       (throw 'nesting 'cpp)) ; nothing sub possible
			      (if (or complete elsed)
				  () ; nothing
				(setq ass nil)
				(save-excursion
				  ; normal indentation
				  (setq curcol (mupad-indent-level))
				  (search-forward ":=" nil t)
				  (if (not (looking-at "[ \t]*\\($\\|\\(/\\(/\\|\*\\)\\)\\|#\\)"))
				      (setq curcol (1+ (current-column)) ;assex t
					    ))
				  ; search for completion of assignment
				  (if (save-excursion (while (not ass)
							(forward-sexp 1)
							(setq ass (or ;(= (following-char) ?\;)
								      ;(= (following-char) ?\:)
								      (looking-at "[ \t]*;")
								      (looking-at "[ \t]*:")
								      (looking-at mupad-end-block-re)
								      (eobp))))
						      (> (point) oldpos))
				      (throw 'nesting 'defun)
				    ))
				))
			    )
			   (;--No known statements
			    (bobp)
			    (setq lastmatch "bobp" lastmatchp 0)
			    (throw 'nesting 'unknown))
			   ))
		   )))

      ;; Return type of block and indent level.
      (cond ((and (> par 0) (> par curcol)) ; Unclosed Parenthesis 
	     (list 'contexp par lastmatch lastmatchp))
	    (t (list type curcol lastmatch lastmatchp))))))

(defun mupad-ci ()
  ""
  (interactive)
  ;(backward-list 1)
  (setq ind (mupad-calculate-indent))
  (setq mov (make-overlay (nth 3 ind) (save-excursion (forward-word 1) (point))))
  (overlay-put mov 'face font-lock-comment-face)
  ;(message (char-to-string (char-syntax (following-char))))
  )

(defun mupad-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case-statements or records."
  (save-excursion
    (beginning-of-line)
    ;(if (looking-at (concat mupad-symbol-re "[ \t]*:="))
    ;   (search-forward ":=" nil t))
    (skip-chars-forward " \t")
    ;(sit-for 0)
    (current-column)))

(defun mupad-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
    (let* ((stcol (save-excursion
		    (re-search-backward "\\(#(\\)\\|# +\\|\\(//\\)\\|\\(/\*\\)" nil t)
		    (1+ (current-column)))))
      (if arg stcol
	(delete-horizontal-space)
	(indent-to stcol))))

(defun mupad-close-statement ()
  "Close the last unclosed statement."
  (interactive)
  (setq ind (mupad-calculate-indent))
  (save-excursion ; subblock
    (while ;(or (string-match mupad-sub-block-re (nth 2 ind))
           ;    (string-match ":=" (nth 2 ind))
	   ;    (not (string= "bobp" (nth 2 ind))))
	(and (> (nth 3 ind) 0) (not (string-match mupad-beg-block-re (nth 2 ind))))
      (goto-char (nth 3 ind))
      (setq ind (mupad-calculate-indent)))
    (if (string-match mupad-beg-block-re (nth 2 ind))
	(setq pat (concat "end_" (nth 2 ind)))
      (setq pat "")))

  (if (not (string= pat ""))
      (progn (delete-horizontal-space)
	     (cond ((not (> (mupad-get-beg-of-line) (nth 3 ind)))
		    (insert (concat " " pat)))
		   ((bolp)
		    (if (looking-at "[ \t]*$")
			(insert pat)
		      (save-excursion (electric-mupad-terminate-line))
		      (insert pat)
		      (electric-mupad-tab)))
		   (t
		    (electric-mupad-terminate-line)
		    (insert pat)))
	     (electric-mupad-tab))
    (message "No unclosed statement found."))
  )

   


;;; bij - neue Funktionen
(defun mupad-fun-to-proc (&optional sende)
  "Schneidet 'fun(...)' aus und gibt '(()->(...))' zurück"
  (interactive)
  (setq now (point))
  (if 'sende
      ()
    (setq sende (mupad-get-end-of-search)))
  (re-search-forward "\\\<fun\\\>" sende t)
  (re-search-backward "\\\<fun\\\>" 1)
  (if (string= (char-to-string (char-syntax (preceding-char))) "\"")
      (prog (message "fun als Zeichenkette")
  	    (sit-for 1)
  	    (if (< (point) now)
  		(message "Keine 'fun' mehr in dieser Datei."))
  	    (goto-char (1+ (point))))
    (save-excursion (setq ende (mupad-next-balanced-brace)))
    (setq anf (point-marker))
    (replace-match "" t t)
    (kill-region anf ende)
    (setq reg (substring (current-kill 0) 1 -1))
    (goto-char ende)
    (insert (concat "(()->(" (concat reg) "))"))
    )
  ;(message "Region %s" reg))
)

(defun mupad-next-balanced-brace ()
  "Gibt die Position der nächsten geschlossenen Klamer zurück
 (innerhalb der Zeile)"
  (interactive)
  (save-excursion 
    (nth 0 (parse-partial-sexp (point) (buffer-size) 0))
    (point-marker)
    ;(sit-for 1)
    )
  )

;(defun mupad-comment-region (start end &optional arg)
;  "Write a # at beginning and end of every line in region.
;Double every # in the region."
;  (interactive "r")
;  (save-excursion
;    (goto-char start)
;    (beginning-of-line)
;    (setq start (point))
;    (goto-char end)
;    (beginning-of-line)
;    (setq end (point))
;    (goto-char start)
;;;; replace the old #'s by °'s (I hope nobody need this...)
;    (goto-char start)
;    (save-excursion
;      (while (re-search-forward "#" end t)
;	(if (not(mupad-within-string))
;	    (replace-match "°"))
;	))
;;;; the new #'s
;    (while (< (point) end)
;      (end-of-line)
;      (setq enl (point))
;      (beginning-of-line)
;      (skip-chars-forward " \t")
;      ;;; not empty lines
;      (if (< (point) enl)
;	  (progn (insert "#")
;		 (setq end (+ 1 end))
;		 (end-of-line)
;		 (insert "#")
;		 (setq end (+ 1 end))
;		 (forward-line 1))
;	(forward-line 1)
;	))
;;;; double the old #'s
;    ;(goto-char start)
;    (save-excursion
;      (while (re-search-backward "°" start t)
;	(if (not(mupad-within-string))
;	    (replace-match "##"))
;	))))

(defun mupad-commented-line ()
  "non-`nil' if exist a # at beginning and end of current line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (< (re-search-forward "^[ \t]*#" (end-of-line) nil) (end-of-line))
	(if (< (re-search-forward "#[ \t]*$" (end-of-line) nil) (end-of-line))
	    (message "line is commented"))
      (message "line is NOT commented")
      )))

(defun uncomment-region (start end)
  "Remove '//' at beginning of each line in region"
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (beginning-of-line)
      (narrow-to-region start end)
      (replace-regexp "^//" "")
      )
    ))
;  "Remove a # at begin and end of every line in region if exist.
;Single every ## in every commented line the region."
;  (interactive "r")
;  (save-excursion
;    (save-restriction
;      (goto-char start)
;      (beginning-of-line)
;      ;(backward-char 2)
;      (setq start (point))
;      (goto-char end)
;      (beginning-of-line)
;      (setq end (point))
;      (narrow-to-region start end)
;;;; search for #'s at begin and end of lines (commented lines)
;      ;(beginning-of-buffer)
;      ;(save-excursion
;      (goto-char start)
;      (while (re-search-forward "^[ \t]*#" end t)
;	(if (not(mupad-within-string))
;	    (progn (backward-delete-char 1)
;		   (end-of-line))
;	  ))
;	;(goto-char start)
;	(while (re-search-backward "#[ \t]*$" start t)
;	  (if (not(mupad-within-string))
;	      ;(replace-string "#" "")
;	      (progn (backward-delete-char -1)
;		     (beginning-of-line))
;	    ));)
;;;; "single" the old ##'s
;      (goto-char start)
;      (save-excursion
;	(while (search-forward "##" end t)
;	  (if (not(mupad-within-string))
;	      (replace-match "°"))
;	  ))
;      (goto-char start)
;      (save-excursion
;	(while (search-forward "°" end t)
;	  (if (not(mupad-within-string))
;	      (replace-match "#"))
;	  ))
;      )))

(defun mupad-tab-line (&optional here)
  "'non-nil' falls die Zeile mit einem Tab beginnt"
  )

(defun mupad-indent-region (start end)
  "Ein Zeichen nach rechts einrücken"
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (save-excursion
      (while (< (point) end)
	(progn (beginning-of-line)
	       (skip-chars-forward " \t")
	       (insert " ")
	       (next-line 1)
	       )))
    ))

(defun mupad-unindent-region (start end)
  "Ein Zeichen nach rechts einrücken"
  (interactive "r")
  (save-excursion
    (goto-char end)
    ;(previous-line 1)
    (setq end (point-marker))
    (goto-char start)
    (save-excursion
      (while (< (point) end)
	(progn (beginning-of-line)
	       (skip-chars-forward " \t")
	       (if (not (bolp))
		   (backward-delete-char-untabify 1))
	       (next-line 1)
	       )))
    ))

;;; bij - aus tex-mode.el
(defun mupad-validate-buffer ()
  "Check current buffer for paragraphs containing mismatched $'s.
As each such paragraph is found, a mark is pushed at its beginning,
and the location is displayed for a few seconds."
  (interactive)
  (let ((opoint (point)))
    (goto-char (point-max))
    ;; Does not use save-excursion
    ;; because we do not want to save the mark.
    (unwind-protect
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point)))
	    (search-backward "\n\n" nil 'move)
	    (or (mupad-validate-region (point) end)
		(progn
		  (push-mark (point))
		  (message "Mismatch found in pararaph starting here")
		  (sit-for 4)))))
      (goto-char opoint))))


;;; bij - aus tex-mode.el
(defun mupad-validate-region (start end)
  "Check for mismatched braces or $'s in region.
Returns t if no mismatches.  Returns nil and moves point to suspect
area if a mismatch is found."
  (interactive "r")
  (let ((failure-point nil) (max-possible-sexps (- end start)))
    (save-excursion
      (condition-case ()
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char start)
	    (while (< 0 (setq max-possible-sexps (1- max-possible-sexps)))
	      (forward-sexp 1)))
	(error
	  (setq failure-point (point)))))
    (if failure-point
	(progn
	  (goto-char failure-point)
	  nil)
      t)))

;;;----------------------------------------------------------------------------
;;;-- Completions (from lisp-mode) --------------------------------------------
;;;----------------------------------------------------------------------------
(defun mupad-complete-symbol ()
  "Perform completion on MuPAD object preceding point.
Compare that symbol against the known MuPAD objects.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point-marker))
	 (beg (save-excursion (backward-word 1) (point)))
	 (beh (save-excursion
		(if (= (preceding-char) ?: )
		    (if (= (progn (save-excursion (forward-char -1) (preceding-char))) ?: )
			(1- (1- end))
		      (1- end))
		  end)))
	 (dom (save-excursion
		(if (member (buffer-substring beg beh) mupad-libraries-list)
		    (if (string= (buffer-substring beh end) "::")
			1
		      (if (string= (buffer-substring beh end) ":")
			  (progn (insert "")
				 1)))
		  (save-excursion
		    (goto-char beg)
		    (backward-word 1)
		    (if (member (buffer-substring (point) (progn (forward-word 1) (point)))
				mupad-libraries-list)
			2 ())))))
	 (beg (if (= (if (not dom) 0 dom) 2)
		  (progn (save-excursion (goto-char beg) (backward-word 1) (point)))
		beg))
	 (pattern (buffer-substring beg end))
	 (completion (if dom (try-completion pattern mupad-libraries-completion-list)
		       (try-completion pattern mupad-completion-list))))
    (cond ((eq completion t) (delete-other-windows)) ;; fertig
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion) (delete-other-windows))
	  (t
	   (message "Making completion list...")
	   (let ((clist (all-completions pattern (if dom
						    mupad-libraries-completion-list
						  mupad-completion-list)))
		 (completion-fixup-function
		  (function (lambda () (if (save-excursion
					     (goto-char (max (point-min) (- (point) 4)))
					     (looking-at " <f>"))
					   (forward-char -4))))))
	     (setq clist (sort clist 'string<))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list clist))
	     ;(momentary-string-display (concat clist)
	     ;  		        end)
	     ;(completing-read pattern clist)
	     )
	   (message "Making completion list...%s" "done")))))

;;;----------------------------------------------------------------------------

(require 'font-lock) ;;; Standard-Emacs

;;; bij - own faces

;(setq orig-font-lock-face-attributes font-lock-face-attributes)

(setq font-lock-keywords-case-fold-search t)

;(defun mupad-color ()
;  " "
;  (interactive)
;  (setq col (face-property 'default 'background 'global))
;  (message "Color: %s" col)
;  ;(message "Color: %s" (x-color-values (face-foreground 'default)))
;  )

(defun mupad-color ()
  " "
  (interactive)
  (font-lock-fontify-syntactically-region (point-min) (point-max))
  )

(if (< emacs-major-version 20)
    (progn
      (setq font-lock-face-attributes
      ;(list
	    '((mupad-font-lock-comment-face "#BBBBBB") ; Grey
	      (mupad-font-lock-string-face  "#38D238") ; LimeGreen
	      (mupad-font-lock-keyword-face "#3030DD") ; MediumBlue
	      (mupad-font-lock-options-face "#C03030") ; Optionen
	      (mupad-font-lock-function-name-face "#7080A0") ; 
	      (mupad-font-lock-domain-face  "#C07030") ; Domains
	     ;(mupad-font-lock-variable-name-face "#B22222") ; FireBrick
	      (mupad-font-lock-type-face "#20A040") ; MediumSeaGreen
	      (mupad-font-lock-info-face "#DAA520") ; GoldenRod
	      )
	    )
      )

  (defvar mupad-font-lock-options-face 'mupad-font-lock-options-face "MuPAD-Optionen")
  (defface mupad-font-lock-options-face
    '((((class grayscale) (background light))
       (:foreground "DimGray" :bold t :italic t))
      (((class grayscale) (background dark))
       (:foreground "LightGray" :bold t :italic t))
      (((class color) (background light)) (:foreground "#C03030"))
      (((class color) (background dark)) (:foreground "#BBBBBB"))
      (t (:bold t :italic t)))
    "Font Lock mode face used to highlight comments."
    :group 'mupad-font-lock-highlighting-faces)

  (defvar mupad-font-lock-domain-face 'mupad-font-lock-domain-face "MuPAD-Domains")
  (defface mupad-font-lock-domain-face
    '((((class grayscale) (background light))
       (:foreground "DimGray" :bold t :italic t))
      (((class grayscale) (background dark))
       (:foreground "LightGray" :bold t :italic t))
      (((class color) (background light)) (:foreground "#C07030"))
      (((class color) (background dark)) (:foreground "#C07030"))
      (t (:bold t :italic t)))
    "Font Lock mode face used to highlight comments."
    :group 'mupad-font-lock-highlighting-faces)

  (defvar mupad-font-lock-info-face 'mupad-font-lock-info-face "MuPAD-Userinfos")
  (defface mupad-font-lock-info-face
    '((((class grayscale) (background light))
       (:foreground "DimGray" :bold t :italic t))
      (((class grayscale) (background dark))
       (:foreground "LightGray" :bold t :italic t))
      (((class color) (background light)) (:foreground "#DAA520"))
      (((class color) (background dark)) (:foreground "#DAA520"))
      (t (:bold t :italic t)))
    "Font Lock mode face used to highlight comments."
    :group 'mupad-font-lock-highlighting-faces)

  (if isXEmacs
      (progn
	(defvar mupad-font-lock-comment-face 'mupad-font-lock-comment-face "MuPAD-Kommentare")
	(defface mupad-font-lock-comment-face
	  '((((class grayscale) (background light))
	     (:foreground "DimGray" :bold t :italic t))
	    (((class grayscale) (background dark))
	     (:foreground "LightGray" :bold t :italic t))
	    (((class color) (background light)) (:foreground "#A0B0B0"))
	    (((class color) (background dark)) (:foreground "#AAAAAA"))
	    (t (:bold t :italic t)))
	  "Font Lock mode face used to highlight comments."
	  :group 'mupad-font-lock-highlighting-faces))
    (defvar mupad-font-lock-comment-face 'mupad-font-lock-comment-face "MuPAD-Kommentare")
    (defface mupad-font-lock-comment-face
      '((((class grayscale) (background light))
	 (:foreground "DimGray" :bold t :italic t))
	(((class grayscale) (background dark))
	 (:foreground "LightGray" :bold t :italic t))
	(((class color) (background light)) (:foreground "#B0B0B0"))
	(((class color) (background dark)) (:foreground "#AAAAAA"))
	(t (:bold t :italic t)))
      "Font Lock mode face used to highlight comments."
      :group 'mupad-font-lock-highlighting-faces)
    )

  (defvar mupad-font-lock-string-face 'mupad-font-lock-string-face "MuPAD-Zeichenketten")
  (defface mupad-font-lock-string-face
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "#38D238"))
      (((class color) (background dark)) (:foreground "#38D238"))
      (t (:italic t)))
    "Font Lock mode face used to highlight strings."
    :group 'mupad-font-lock-highlighting-faces)

  (defvar mupad-font-lock-keyword-face 'mupad-font-lock-keyword-face "MuPAD-Schlüsselwörter")
  (defface mupad-font-lock-keyword-face
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "#3030DD"))
      (((class color) (background dark)) (:foreground "#3030DD"))
      (t (:bold t)))
    "Font Lock mode face used to highlight keywords."
    :group 'mupad-font-lock-highlighting-faces)
  
  (defvar mupad-font-lock-function-name-face 'mupad-font-lock-function-name-face "MuPAD-Funktionen")
  (defface mupad-font-lock-function-name-face
    '((((class color) (background light)) (:foreground "#7080A0"))
      (((class color) (background dark)) (:foreground "#7080A0"))
      (t (:inverse-video t :bold t)))
    "Font Lock mode face used to highlight function names."
    :group 'mupad-font-lock-highlighting-faces)
  
  (defvar mupad-font-lock-type-face 'mupad-font-lock-type-face "MuPAD-Datentypen")
  (defface mupad-font-lock-type-face
    '((((class grayscale) (background light)) (:foreground "Gray90" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "#20A040"))
      (((class color) (background dark)) (:foreground "#20A040"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight types."
    :group 'mupad-font-lock-highlighting-faces)
  
  )


;(require 'mupad-colors)

;(add-hook 'emacs-lisp-mode-hook 'turn-on-mupad-font-lock)
;(setq-default mupad-font-lock-mode t)
;(setq-default mupad-font-lock-use-fonts nil)
;(setq-default mupad-font-lock-use-colors t)
;(setq-default mupad-font-lock-use-maximal-decoration nil)
;(setq-default mupad-font-lock-mode-enable-list nil)
;(setq-default mupad-font-lock-mode-disable-list nil)

(defconst mupad-font-lock-keywords (purecopy
  (list
   ; alle Domains (vorgeladene)
   '("\\<\\(Ax\\(iom\\(Constructor\\)?\\)?\\|CF\\|Cat\\(egory\\(Constructor\\)?\\)?\\|Dom\\(ainConstructor\\)?\\|ERROR\\|NUMERIC\\|Network\\|O\\|Pref\\|RGB\\|Series\\|Sum\\|Type\\|adt\\|combinat\\|domains\\|faclib\\|fp\\|gcdlib\\|generate\\|groebner\\|\\(int\\|match\\)lib\\|io\\|lin\\(alg\\|opt\\)\\|listtools\\|misc\\|module\\|num\\(eric\\|lib\\)\\|ode\\|orthpoly\\|piecewise\\|plotlib\\|property\\|rec\\(tform\\)?\\|\\(share\\|solve\\)lib\\|st\\(ats\\|dlib\\|ring\\|udent\\)\\|transform\\)\\>" 0 mupad-font-lock-domain-face t)

   '("\\<\\(A\\(daptive\\|ll\\(data\\)?\\|lways\\|ny\\|pp\\(end\\|rox\\)\\|ttached\\|xes\\(Scaling\\)?\\)\\|BUTCHER6\\|BackSubstitution\\|Banded\\|Beam\\|Bin\\(ary\\)?\\|Bo\\(ttom\\|x\\)\\|CameraPoint\\|Capacity\\|Center\\|Co\\(effs\\|lor\\(Patches\\|s\\)?\\|lumn\\|mplete\\|ncave\\|ntours\\|nvex\\|rner\\)\\|Curves?\\|Cyclic\\|DebugOnTheFly\\|DegInvLexOrder\\|Degree\\|DegreeOrder\\|Delete\\|Diagonal\\|Dimension\\|DomainsOnly\\|DualPrices\\|EULER1\\|Error\\|Eweight\\|Exact\\|Expr\\|Extended\\|Filled\\|FilledCircles\\|First\\|Flat\\|FloatFormat\\|FocalPoint\\|Format\\|Fr\\(ames\\|obenius\\)\\|G\\(C\\|L\\|T\\)\\|Gauss\\(Chebyshev\\|Legendre\\|Tschebyscheff\\)?\\|Gif\\|Grid\\|Height\\|HiddenLine\\|In\\(clude\\|dependent\\|dex\\(List\\)?\\|finity\\|tMod\\|teractive\\)\\|Label\\(ing\\|s\\)\\|Laurent\\|Le\\(ft\\|ngth\\|xOrder\\)\\|Li\\(ne\\(Width\\)?\\|st\\)\\|Log\\|MaxDegree\\|Mesh\\|MinorExpansion\\|Mod\\(e\\|uleTrace\\)\\|Multiple\\|NC\\|Na\\(med?\\|ry\\|tural\\)\\|New\\(tonCotes\\)?\\|No\\(Check\\|Errors\\|\\(Left\\|Right\\)Vectors\\|nNe\\(gative\\|sted\\)\\|ne\\|tAKnot\\)\\|O\\(nly\\|pen\\|rigin\\)\\|P\\(ath\\|eriodic\\|iechart\\(3d\\)?\\|lain\\|lotDevice\\|oint\\(Style\\|Width\\)\\|olyExpr\\|ost\\(Map\\|fix\\)\\|re\\(Map\\|fix\\|tty\\)\\|rincipalValue\\|roc\\|uiseux\\)\\|Qu\\(iet\\|o\\)\\|Ra\\(ster\\|tExpr\\)\\|Re\\(cursive\\|m\\|order\\)\\|Right\\|Root\\|Sample\\|Scal\\(es\\|ing\\)\\|Smoothness\\|Special\\|Stepsize\\|Style\\|Surface\\|Symbolic\\(Analysis\\)?\\|System\\|Taylor\\|Te\\(rms?\\|st\\|xt\\)\\|Ti\\(cks\\|tle\\(Position\\|s\\)?\\)\\|To\\(\\(Minus\\)?Infinity\\|Nearest\\|Zero\\)\\|TrailingZeroes\\|Un\\(Constrained\\|ique\\|quoted\\|restricted\\|simplified\\)\\|Up\\|Use\\(PrimeTab\\|r\\)\\|Values\\|VectorOf\\|ViewingBox\\|Vweight\\|Warning\\|Write\\|X11\\|YRange\\|sin\\(cos\\|hcosh\\)\\)\\>" 1 mupad-font-lock-options-face t) ; MuPAD-Optionen

   ; alle DOM_FUNC_ENV
   '("\\<\\(D\\(poly\\)?\\|Im\\|R\\(e\\|ootOf\\)\\|Si\\|_\\(a\\(ct_proc_env\\|nd\\|ssign\\)\\|break\\|c\\(ase\\|oncat\\)\\|div\\(ide\\)?\\|e\\(qual\\|val_entry\\|xprseq\\)\\|fconcat\\|nest\\|or\\(_down\\|_in\\)?\\|i\\(f\\|n\\(dex\\|tersect\\|vert\\)\\)\\|le\\(equal\\|ss\\)\\|m\\(inus\\|od\\|ult\\)\\|ne\\(gate\\|xt\\)\\|not\\|or\\|p\\(arser_config\\|lus\\|ower\\|r\\(ef\\|ocdef\\)\\)\\|quit\\|r\\(ange\\|epeat\\)\\|s\\(eqgen\\|tmtseq\\|ubtract\\)\\|un\\(equal\\|ion\\)\\|userinfo_level\\|while\\)\\|a\\(bs\\|c\\(_match\\|osh?\\|oth?\\|sch?\\)\\|names\\|ppend\\|rgs\\|rray\\|sech?\\|sinh?\\|ssign_elems\\|tanh?\\)\\|bernoulli\\|besselJ\\|besselY\\|beta\\|binomial\\|bool\\|built_in\\|bytes\\|catalan\\|ceil\\|co\\(eff\\|mbine\\|n\\(jugate\\|tains\\|text\\)\\|s\\|sh\\|th?\\)\\|csch?\\|de\\(bug\\|gree\\(vec\\)?\\)\\|di\\(ff\\|log\\|rac\\|vide\\)\\|dom\\(ainfunccall\\|attr\\|type\\)\\|eint\\|erfc?\\|error\\|evalp?\\|exp\\(and\\|r\\|r2text\\)?\\|ext\\(ernal\\|nops\\|op\\|subsop\\)\\|f\\(act\\|close\\|input\\|lo\\(at\\|or\\)\\|open\\|print\\|r\\(ac\\|ead\\)\\|textinput\\|unc\\(_env\\|attr\\)\\)\\|g\\(amma\\|cov\\|en\\(ident\\|poly\\)\\|etpid\\|prof\\)\\|h\\(as\\|eaviside\\|elp\\|istory\\|old\\)\\|i\\(content\\|d\\|factor\\|gamma\\|gcd\\(ex\\)?\\|lcm\\|nde\\(ts\\|x_val\\)\\|nput\\(_filter\\)?\\|nt\\|s\\(prime\\|qrt\\|zero\\)\\)\\|l\\(ast\\|coeff\\|degree\\|e\\(ngth\\|vel\\)\\|imit\\|llint\\|monomial\\|n\\|oad\\(mod\\)?\\|term\\)\\|ma\\(p\\(coeffs\\)?\\|tch\\|x\\)\\|memory\\|min\\|mod\\(p\\|s\\)\\|multcoeffs\\|new\\(_bytecode\\|_domain\\)?\\|nextprime\\|no\\(history\\|ps\\|rm\\(al\\)?\\)\\|nt\\(erms\\|h\\(coeff\\|monomial\\|term\\)\\)\\|null\\|op\\(_\\(fconcat\\|p\\(lusmult\\|ower\\)\\)\\)?\\|p\\(divide\\|hi\\|lot\\|oint\\|oly\\(gon\\|log\\)?\\|rint\\|ro\\(duct\\|tected\\|tocol\\)\\|si\\|time\\)\\|re\\(gister\\|place\\|set\\|turn\\)\\|r\\(ound\\|time\\)\\|se\\(arch\\|ch?\\|lect\\|ries\\)\\|si\\(gn\\|mplify\\|nh?\\)\\|so\\(lve\\|rt\\)\\|s\\(plit\\|qrt\\)\\|str\\(len\\|match\\)\\|subs\\(ex\\|op\\|tring\\)?\\|sum\\|sys\\(\\(un\\)?assign\\|name\\|order\\|tem\\)\\|ta\\(ble\\|nh?\\)\\|tbl2text\\|tcoeff\\|test\\(args\\|type\\)\\|text\\(2\\(expr\\|list\\|tbl\\)\\|input\\)\\|t\\(ime\\|raperror\\|runc\\|ype\\)\\|un\\(assign\\|loadmod\\)\\|userinfo\\|val\\|write\\|z\\(eta\\|ip\\)\\)\\>" 1 mupad-font-lock-function-name-face t)

   ; alle DOM_PROC
   '("\\<\\(Indets\\|RootOf_\\(evala\\|expand_power\\|polysum\\)\\|_\\(check_global\\|constructor\\|print_userinfo\\|seqin\\)\\|al\\(gsolve\\|ias\\|lvalues\\)\\|ass\\(ign\\|ume\\)\\|asympt\\|breakmap\\|changevar\\|co\\(llect\\|nt\\(ent\\|frac\\)\\)\\|de\\(compose\\|nom\\)\\|disc\\(ont\\|rim\\)\\|evalassign\\|expo\\(rt\\|se\\)\\|\\(F\\|f\\)actors?\\|fft\\|fpolylog_\\(\\(b\\|s\\(ln\\)?\\)z\\)\\|gcd\\(ex\\)?\\|getprop\\|hastype\\|hex\\|i\\(fft\\|nfo\\|nsert_ordered\\|quo\\|rem\\|rreducible\\|s\\|thprime\\)\\|lagrange\\(2d\\)?\\|lazy_and\\|lcm\\|lins\\(ert\\|olve\\)\\|load\\(lib\\|proc\\)\\|mapr\\(at\\|ec\\)\\|mkfloat\\|new\\(funcarg\\|purefunc\\)\\|numer\\(_polylog_neg\\)?\\|op\\(erator\\|timize\\)\\|pa\\(rtfrac\\|tchlevel\\|thname\\)\\|pdioe\\|plot\\(2d\\|3d\\|func\\)\\|Poly\\|poly\\(2list\\|log_neg\\)\\|powermod\\|pr\\(impart\\|o\\(file\\|perties\\|tect\\)\\)\\|ra\\(dsimp\\|nd\\(om\\|poly\\)\\|tionalize\\)\\|re\\(ad\\|sultant\\|vert\\|write\\)\\|se\\(q\\|tuserinfo\\)\\|spline\\|sqrfree\\|taylor\\|un\\(alias\\|assume\\|export\\|loadlib\\|protect\\)\\|version\\|warning\\)\\>" 0 mupad-font-lock-function-name-face t)

   ; alle DOM_ Typen
   '("\\<\\(DOM_\\(ARRAY\\|BOOL\\|COMPLEX\\|DOMAIN\\|EX\\(EC\\|PR\\)\\|FAIL\\|FLOAT\\|FUNC_ENV\\|IDENT\\|\\(PO\\)?INT\\|LIST\\|NIL\\|NULL\\|POLY\\(GON\\)?\\|PROC\\(_ENV\\)?\\|RAT\\|SET\\|STRING\\|TABLE\\|VAR\\)\\)\\>" 1 mupad-font-lock-type-face t)

   '("\\<\\(TRUE\\|FALSE\\|UNKNOWN\\|FAIL\\|NIL\\|infinity\\)\\>" 0 mupad-font-lock-type-face t)
   ;'("\\(\\\w+\\)\\W()" 1 mupad-font-lock-keyword-face t)
   ;'("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)\\W" 1 mupad-font-lock-function-name-face t)
   ;'("\\W\\(\\.\\|+\\|-\\|\\*\\|/\\|,\\)\\W" 1 mupad-font-lock-info-face t)

   ;alle Schluesselwoerter
   '("\\<\\(and\\|axiom\\|b\\(egin\\|reak\\)\\|ca\\(\\(tegory\\)\\|\\(se\\)\\)\\|div\\|do\\(\\(main\\)\\|\\(wnto\\)\\)?\\|E\\|el\\(if\\|se\\)\\|end\\(_\\(axiom\\|ca\\(\\(tegory\\)\\|\\(se\\)\\)\\|domain\\|for\\|if\\|proc\\|repeat\\|while\\)\\)?\\|FA\\(IL\\|LSE\\)\\|f\\(or\\|rom\\)\\|I\\|i\\(f\\|n\\(tersect\\)?\\)\\|local\\|m\\(inus\\|od\\)\\|n\\(ame\\|ext\\|ot\\)\\|NIL\\|o\\(f\\|ption\\|r\\|therwise\\)\\|proc\\|quit\\|repeat\\|step\\|t\\(hen\\|o\\)\\|TRUE\\|un\\(ion\\|til\\)\\|UNKNOWN\\|while\\|save\\)\\>" 0 mupad-font-lock-keyword-face t)

   ; alle Operator-Zeichen
   '("\\.\\|\\+\\|->?\\|\\*\\|/\\|,\\|<\\|\\(<=\\)\\|>\\|\\(>=\\)\\|\\(:?=\\)\\|\\(<>\\)\\|\\(\\.\\.\\)\\|\\^\\|\\(\\<&\\w*\\>\\)\\|\\@\\|\\(::?\\)\\|\\$\\|;\\|(\\|)\\|\\[\\|\\]" 0 mupad-font-lock-keyword-face t)
   ; alle vordefinierten Operatoren
   '("\\<\\(and\\|or\\|union\\|minus\\|intersect\\|mod\\|div\\|not\\)\\>" 0 mupad-font-lock-keyword-face t)
   ;'("[^\\\\\\`]\\#\\([^#]*\\)\\#" 0 mupad-font-lock-comment-face t)
   ;'("`[^\\\\]\\#\\([^#]*\\)\\#" 1 mupad-font-lock-comment-face t)
   ;'("^[ \t]*\\#\\([^#]*\\)\\#" 0 mupad-font-lock-comment-face t)
   ;;'("[^\\\\\w]\"\\([^\"]*\\)\"\\\W" 1 mupad-font-lock-string-face t)
   '("\\\"\\([^\\(\\\\\\\"\\)]*\\)\\\"" 0 mupad-font-lock-string-face t)
   ;'("\\W\\\"\\([^\\\"]*\\)\\\"\\W" 1 mupad-font-lock-string-face t)
   ;'("\\W\\\"\\([^\\\"]*\\)\\\"\\W" 0 mupad-font-lock-string-face t)
   ;'("\\(\\\w+\\)(\\([^)\n]+\\))" 2 mupad-font-lock-variable-name-face t)
   '("\\(\\<userinfo\\>(\\)\\([^;]+\\);" 0 mupad-font-lock-info-face t)
   '("\\<userinfo([^)]+)\\>" 1 mupad-font-lock-info-face t)
   '("\\<userinfo([^)]+)\\>" 2 mupad-font-lock-info-face t)
   '("#\\([^#]*\\)#" 0 mupad-font-lock-comment-face t)
   ;'("#\\([^#]*\\)#" 1 mupad-font-lock-comment-face t)
   '("//.*$" 0 mupad-font-lock-comment-face t)
   ;"\\s\"\\|/[*/]"
   ;'("/\*\\(.*\\)\*/" 0 mupad-font-lock-comment-face t)
   ;'("/\\\*\\([^\\(\\\*/\\)]*\\)\\\*/" 1 mupad-font-lock-comment-face t)
   ;'("/\\\*\\([^\\(\\\*/\\)]*\\)\\\*/" 0 mupad-font-lock-comment-face t)
   ;'("/\\\*\\([^\\(\\\*/\\)]*\\)\\\*/" 1 mupad-font-lock-comment-face t)
   ;'("//.*$" 1 mupad-font-lock-comment-face t)

   "Additional expressions to highlight in MuPAD mode.")))

(provide 'mupad-mode)

;;; mupad.el ends here
