;;; settings.el --- general settings

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2017-October-30 (06:28)
;;----------------------------------------------

;; (note that some more settings are are also made in xwin.el)
;; try "M-x list-options" to see more options avail


;; turn off blinking of cursor:
(and (fboundp 'blink-cursor-mode) (blink-cursor-mode (- (*) (*) (*))))

;; avoid stupid gtk file dialog:
(setq dialog-box nil)
(setq use-file-dialog nil)
;; Better: use emacs-x11:
;;   export EMACS_TOOLKIT=x11
;; Possible values for EMACS_TOOLKIT are nox, gtk, or x11
;; May want to edit the script /usr/bin/emacs

;; case-fold-search while searching:
;;   either nil, t, or 'yes.  'yes means the same as t except that mixed
;;   case in the search string is ignored.
;;(setq case-fold-search t)

;; Regexp to recognize a character in an abbreviation or expansion:
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;; Regexp matching URLs.
;; Set to nil to disable URL features in find-file-at-point (ffap):
;;(setq ffap-url-regexp nil)

;; height of compilation window:
(setq compilation-window-height 40) ; default==half vert dim

;; Avoid the "tooltips":
;;(setq tooltip-delay 9999)
;;(tooltip-mode -1)

;; disable tool bar:
(and (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))

;; Silently accept specifications of local variables in files:
(setq enable-local-variables :all)
;; SECURITY NOTE: comment out the line in order to be always
;; asked whether to set the local variables given.
;; Same for local eval:
(setq enable-local-eval t)

;; Width used with function fill-paragraph (M-q):
;;(setq fill-column 80)

;; A more fine-grained minibuffer completion feedback scheme.
;; Prospective completions are concisely indicated within the
;; minibuffer itself, with each successive keystroke.
(icomplete-mode t)

;; Iswitchb global minor mode provides convenient switching between
;; buffers using substrings of their names.
;;(iswitchb-mode t)
;; marked as obsolete on Debian jessie, 2015-May-04
;; icomplete-mode (above) is a fine substitute,
;; see http://emacswiki.org/emacs/IswitchBuffers

;; If non-nil, `kill-line' with no arg at beg of line kills the whole line.
;;(setq kill-whole-line t)

;; if the argument is positive/negative, turn on/off menu bars:
;; (applies to all frames that exist and frames to be created in the future)
;;(menu-bar-mode -1)
(and (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

;; Group the buffers by the major mode groups on C-down-mouse-1?
;; This number which determines whether C-down-mouse-1
;; will split the buffer menu by the major modes (see
;; `mouse-buffer-menu-mode-groups') or just by menu length.
;; Set to 1 (or even 0!) if you want to group by major mode always, and to
;; a large number if you prefer a mixed multitude.  The default is 4.
(setq mouse-buffer-menu-mode-mult 100)

;; Non-nil means `forward-sexp', etc., should treat comments as whitespace:
(setq parse-sexp-ignore-comments t)


;; Maximum buffer size (in characters) for line number display
;; If the buffer is bigger than this, the line number does not
;; appear in the mode line:
(setq line-number-display-limit 1000000)

;; show line number:
(setq line-number-mode t)

;; linum-mode everywhere:
(global-linum-mode)


;; Non-nil if searches should ignore case
(setq case-fold-search t)

;; show column number:
(setq column-number-mode t)

;; Non-nil means query-replace should preserve case in replacements
(setq case-replace t)

;; your feedback goes to:
;;(setq user-mail-address " arndt (AT) jjj.de")

;; The name of your organization, as a string.
;; The `ORGANIZATION' environment variable is used instead if defined.
;;(setq gnus-local-organization "")


;; Non-nil means try to flash the frame to represent a bell.
(setq visible-bell t)

;; no initial message:
(setq inhibit-startup-message t)


;; don't add new lines with arrow down:
(setq next-line-add-newlines nil)

;; List of tab stop positions used by `tab-to-tab-stops'.
;; This should be a list of integers, ordered from smallest to largest.
;;(setq tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))

;; Distance between tab stops (for display of tab characters), in columns.
;; Automatically becomes buffer-local when set in any fashion.
;;(setq default-tab-width 8)

;; no name~ backup files
(setq make-backup-files nil)

;; ??
(put 'eval-expression 'disabled nil)

;;; http://www.emacswiki.org/emacs/DeleteSelectionMode
;; delete-selection-mode: an interactive compiled Lisp function.
;; (delete-selection-mode ARG)
;; Toggle Delete Selection mode.
;; When ON, typed text replaces the selection if the selection is active.
;; When OFF, typed text is just inserted at point.
(setq delete-selection-mode nil)

;; Indentation can insert tabs if this is non-nil:
;; Setting this variable automatically makes it local to the current buffer.
;(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Value of t says silently ensure a file ends in a newline when it is saved.
;; Non-nil but not t says ask user whether to add a newline when there isn't one.
;; nil means don't add newlines.
(setq require-final-newline 1)

;; disable scroll bar:
;;(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; The number of lines to try scrolling a window by when point moves out.
;; If that fails to bring point back on frame, point is centered instead.
;; If this is zero, point is always centered after it moves off frame.
;;(setq scroll-step 15)
(setq scroll-step 1); better for proof reading texts

;; The number of columns to try scrolling a window by when point moves out.
;; If that fails to bring point back on frame, point is centered instead.
;; If this is zero, point is always centered after it moves off frame.
(setq hscroll-step 20)

;; If non-nil, do a nonincremental search instead if exiting immediately.
;; Actually, `isearch-edit-string' is called to let you enter the search
;; string, and RET terminates editing and does a nonincremental search.
(setq search-nonincremental-instead t)

;; Non-nil means incremental search highlights the current match.
(setq search-highlight t)

;; Non-nil means to highlight words during query replacement.
(setq query-replace-highlight t)

;; If non-nil, split windows keeps the original point in both children.
;; This is often more convenient for editing.
;; If nil, adjust point in each of the two windows to minimize redisplay.
;; This is convenient on slow terminals, but point can move strangely.
(setq split-window-keep-point nil)

;;; http://www.emacswiki.org/emacs/TransientMarkMode
;; Non-nil means deactivate the mark when the buffer contents change
;; Non-nil also enables highlighting of the region whenever the mark is active.
;; The variable `highlight-nonselected-windows' controls whether to highlight
;; all windows or just the selected window.
(setq transient-mark-mode t)
;; Note:
;; Trying to disable Transient Mark mode while CUA mode is enabled
;; does not work; if you only want to highlight the region when it
;; is selected using a shifted movement key, set
;;`cua-highlight-region-shift-only'.
(setq cua-highlight-region-shift-only t)


;; Non-nil means you can use the mark even when inactive.
;; This option makes a difference in Transient Mark mode.
;; When the option is non-nil, deactivation of the mark
;; turns off region highlighting, but commands that use the mark
;; behave as if the mark were still active.
(setq mark-even-if-inactive nil)

;; Modes for which Font Lock mode is automagically turned on.
;; ? does not work ?
;;(setq font-lock-global-modes (sh-mode shell-script-mode))

;; The pseudo-pattern that governs the way a time of day is formatted.
;;(setq  calendar-time-display-form
;;       '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

;; Set to t if you want to highlight AmSLaTeX commands.
;;(setq hilit-AmSLaTeX-commands nil)


;;(require 'tex-site)
;;(setq TeX-auto-save t)
;;(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;(setq ispell-change-dictionary 'american)
;;(setq ispell-dictionary "american")
(setq ispell-check-comments nil)

(setq jj-tex-skip
      '(
        ("\\\\NS" ispell-tex-arg-end 1) ;; No Spell
;;        ("\\\\jjindex" ispell-tex-arg-end 1)
        ("\\\\jjcite" ispell-tex-arg-end 1)
        ("\\\\jjPcite" ispell-tex-arg-end 2)
        ("\\\\jjlabel" ispell-tex-arg-end 1)
        ("\\\\jjformula" ispell-tex-arg-end 1)
        ("\\\\jjref" ispell-tex-arg-end 1)
        ("\\\\jjvref" ispell-tex-arg-end 1)
        ("\\\\jjVref" ispell-tex-arg-end 1)
        ("\\\\jjseqref" ispell-tex-arg-end 1)
        ("\\\\jjpageref" ispell-tex-arg-end 1)
;;        ("\\\\jjfxtref" ispell-tex-arg-end 2);; obsolete
        ("\\\\jjfxtclref" ispell-tex-arg-end 2);; FXT class
        ("\\\\jjfxtdref" ispell-tex-arg-end 1);; FXT demo
        ("\\\\jjfxtfref" ispell-tex-arg-end 1);; FXT file
        ("\\\\jjfxtaref" ispell-tex-arg-end 1);; FXT math data
        ("\\\\jjhfloatfref" ispell-tex-arg-end 1)
        ("\\\\jjfile" ispell-tex-arg-end 1)
        ("\\\\jjtodo" ispell-tex-arg-end 1)
        ("\\\\jjTodo" ispell-tex-arg-end 1)
        ("\\\\jjTODO" ispell-tex-arg-end 1)
        ("\\\\jjxinput"  ispell-tex-arg-end 1)
;;        ("\\\\verb@[^@]+@") ;; for \verb@some thing@
        ("\\\\verb\"[^\"]+\"") ;; for \verb"some thing"
        ) )

(setq jj-tex-skip2
      '(
;;        ("bla" . "\\\\end{bla}")%% for \begin{bla} ... \end{bla}
        ("listing" . "\\\\end{listing}")
;;        ("\\\\begin{listing}{[0-9]+}" . "\\\\end{listing}")
        ("verbatim" . "\\\\end{listing}")
        ) )

;
;; old mechanism:
;;(add-to-list 'ispell-tex-skip-alists jj-tex-skip)
;
;; new mechanism:
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists) jj-tex-skip)
       (append (cadr ispell-tex-skip-alists) jj-tex-skip2)
       )
      )



;;(setq ispell-skip-region-alist
;;      (list
;;       (append ispell-skip-region-alist jj-region-skip)
;;       )
;;      )
;; ==> eval: Symbol's function definition is void: ispell-words-keyword

;; If non-`nil', resize the minibuffer so its entire contents are visible.
(setq resize-minibuffer-mode t)

;; Non-nil means vertical motion starting at end of line keeps to ends of lines.
;; This means moving to the end of each line moved onto.
;; The beginning of a blank line does not count as the end of a line.
(setq track-eol nil)



;; Display message if matching open paren is offscreen.
(setq paren-message-offscreen nil)

;; For stig-paren.el:
;; Make noise if the cursor is at an unmatched paren.
;; If t, then typing or passing over an unmatched paren will ring the bell.
;; If nil, then the bell will not ring even if an unmatched paren is typed.
;; If neither t or nil, then the bell will not ring when the cursor moves over
;; unmatched parens but will ring if one is typed.
(setq paren-ding-unmatched nil)

;; If this is non-nil, it specifies how Emacs should “ring the bell.”
;; Its value should be a function of no arguments. If this is non-nil,
;; it takes precedence over the visible-bell variable.
(setq ring-bell-function (lambda () (message "***** BEEP ***** (ding).")))
;; Mostly to turn off annoying behaviour with
;; - mismatched parens
;; - attempts to scroll past begin or end


;; Let focus follow mouse:
;;(load "follow-mouse")
;;(turn-on-follow-mouse);; causes problems when moving modeline between buffers
;;(turn-off-follow-mouse)
;;(setq follow-mouse-deselect-active-minibuffer nil)
;;(setq follow-mouse-auto-raise-frame nil))


;; drives you insane:
;;(type-break-mode)

;; don't hide DOS lineendings (^M):
(setq inhibit-eol-conversion t)

;; Suppress message in *scratch* buffer:
(setq initial-scratch-message "")
;; and want text mode there:
(setq initial-major-mode 'text-mode)


;;
(custom-set-faces
;; '(highline-face ((t (:background "grey95"))))

 '(subscript ((default) (nil nil)))
 '(superscript ((default) (nil nil)))
 '(tex-verbatim ((default) (nil nil)))
;; '(tex-verbatim "courier")
 )


;;(add-to-list 'custom-theme-load-path "~/.jjemacs/themes/")
;;(setq custom-theme-directory "~/.jjemacs/themes/")

;; when sorting, do not gather all capital letters first:
(setq sort-fold-case t)


;;; disable URL features in ffap:
;;(setq ffap-url-regexp nil)

;; make OEIS A-numbers valid URLs: (no idea how to get that right)
;;(setq ffap-something-something '("A123456" . "firefox https://oeis.org/%s") )
;; see goto-address-mode browse-url-url-at-point thing-at-point thingatpt.el


;; defined in face-menu.el, affects list-colors-display:
;;(setq list-colors-sort 'rgb)

;;(provide 'settings)

;;; settings.el ends here
