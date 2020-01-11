;;; sql-upcase-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "sql-upcase" "sql-upcase.el" (23981 27791 177111
;;;;;;  727000))
;;; Generated autoloads from sql-upcase.el

(autoload 'sql-upcase-mode "sql-upcase" "\
Automatically upcase SQL keywords as text is inserted in the buffer.

Intended to be enabled via `sql-mode-hook' and/or `sql-interactive-mode-hook'.

Note that this can be a little aggressive in `sql-interactive-mode'. Although
output from the inferior process is ignored, all other text changes to the
buffer are processed (e.g. cycling through the command history).

\(fn &optional ARG)" t nil)

(autoload 'sql-upcase-region "sql-upcase" "\
Upcase SQL keywords within the marked region.

Keywords overlapping BEGINNING will be upcased.
Keywords overlapping END will not be upcased.

\(fn BEGINNING END)" t nil)

(autoload 'sql-upcase-buffer "sql-upcase" "\
Upcase all SQL keywords in the buffer.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sql-upcase-autoloads.el ends here
