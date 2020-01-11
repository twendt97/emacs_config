;;; funcs-tex.el --- function definitions for (La)TeX

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:09)
;;----------------------------------------------


(defun jjj-synctex-get-pdf-page (fname sline pdfname)
  "Find out pdf-page we are looking at, using synctex."

  ;;; Problem here, synctex is a buggy pile of crap:
  ;;; - silent failure, no indication what went wrong, even returning 0
  ;;; - multiple Page numbers can appear
  ;;; - no useful documentation
  ;;; So ... this works for the TeX-master but for no other file

  (interactive)
;;   cmd-str (concat "synctex view -i " sline ":0:" fname " -o " pdfname " | grep Page | sed 's/Page://;'")
       (setq my-pwd (cur-directory))
       (setq cmd-str (concat "synctex view -i " sline ":0:" fname " -o " pdfname " | grep Page | sed 's/Page://;' | tail -n 1"))
       ;; full name with path:
;;       (setq cmd-str (concat "synctex view -i " sline ":0:" my-pwd "" fname " -o " pdfname " | grep Page | sed 's/Page://;' | tail -n 1"))

       (message "jjj-synctex-get-pdf-page: fname=[%s]" fname);; for debug
       (message "jjj-synctex-get-pdf-page: sline=[%s]" sline);; for debug
       (message "jjj-synctex-get-pdf-page: pdfname=[%s]" pdfname);; for debug
       (message "jjj-synctex-get-pdf-page: my-pwd=[%s]" my-pwd);; for debug
       (message "jjj-synctex-get-pdf-page: cmd-str=[%s]" cmd-str);; for debug
       (shell-command-to-string cmd-str )
       )

;;
(defun jj-pdf-at-pos ()
  "Launch pdf-viever and jump to position corresponding to file and line.
This function will work with (La)TeX files and source code specials.
If the variable `pdf-file' is set, it should be the basename of the pdf file.
Otherwise the variable `TeX-master' should be the basename of the pdf file,
And the variable `MyRelDir' (if set) should be the directory between the
pdf file and the buffer file (often ../ or ./)."
  (interactive)
  (let (
        (TM) ;; local copy of TeX-master
        (cmd "okular")
;;        (cmd "zathura")
        (sline (number-to-string (jjj-get-line-number)));; line number in source
        (fname (file-relative-name buffer-file-truename));; TeX-file we are looking at
        )
    (if (boundp 'pdf-file)
        (setq TM pdf-file)
      (if (not (boundp 'TeX-master))
          (progn
            (message "Variable TeX-master not set, trying basename(.pdf)")
            (setq TM (jjj-base-name (cur-file)))
            )
        (progn
          (if (eq TeX-master nil)
              (progn
                (message "Variable TeX-master is nil, trying basename(.pdf)")
                (setq TM (jjj-base-name (cur-file)))
                )
            (setq TM TeX-master)
            ) ) ) )

    (if (boundp 'MyRelDir) ;; can set to "" with LaTeX3
;;        (setq frel (concat MyRelDir "/" fname)) ;; LaTeX2
        (setq fname (concat "./" fname)) ;; LaTeX3
      )

    (setq pdfname (concat TM ".pdf"))
    (setq pdfpage (jjj-synctex-get-pdf-page fname sline pdfname))

    (message "jj-pdf-at-pos: pdfname=[%s]" pdfname);; for debug
    (message "jj-pdf-at-pos: TM=[%s]" TM);; for debug
    (message "jj-pdf-at-pos: cmd=[%s]" cmd);; for debug
    (message "jj-pdf-at-pos: fname=[%s]" fname);; for debug
    (message "jj-pdf-at-pos: sline=[%s]" sline);; for debug
    (message "jj-pdf-at-pos: pdfpage=[%s]" pdfpage);; for debug

    (if (not (file-exists-p pdfname))
        (error (concat "No pdf file (" pdfname ")"))
      ; else launch pdf-viewer
      (call-process cmd ; PROGRAM
                    nil ; INFILE
                    0   ; BUFFER
                    nil ; DISPLAY
                    ; rest: ARGS
                    pdfname ;; full name of pdf file (usually==TeX-master)
;;                    "--synctex-forward=450:0:fam-curves-bridges.tex -o fam-curves-bridges.pdf" ;; zathura
                    ;; expects the same format as specified for synctex's view -i
                    ;; e.g.: synctex view -i 450:0:fam-curves-bridges.tex -o fam-curves-bridges.pdf
;;                    "--page" "3"  ;; okular
;;                    "--page" "$(" "synctex" "view" "-i" sline ":0:" TM "-o" pdfname "|" "grep" "Page" "|" "sed" "'s/Page://;'" ")"  ;; okular
                    "--page" pdfpage  ;; okular
                    ;; need to decode synctex output, see synctex help view
;;                    "-geom" "880x1100-0-0"
;;                    "-geom" "997x1498--70--200"
;;                    "-expert" "-s" "6"
;;                    "-expert" "-s" "5"
;;                    "-gamma" "1.8"
;;                    "-sourceposition" (concat sline " " fname)
                    )
      )
    )
  )


;;
(defun jj-xdvi-at-pos ()
  "Launch xdvi and jump to position corresponding to file and line.
This function will work with (La)TeX files and source code specials.
If the variable `dvi-file' is set, it should be the basename of the dvi file.
Otherwise the variable `TeX-master' should be the basename of the dvi file,
And the variable `MyRelDir' (if set) should be the directory between the
dvi file and the buffer file (often ../ or ./)."
  (interactive)
  (let (
        (TM) ;; local copy of TeX-master
        (cmd "xdvi")
        (sline (number-to-string (jjj-get-line-number)));; line number in source
        (fname (file-relative-name buffer-file-truename))
        )
    (if (boundp 'dvi-file)
        (setq TM dvi-file)
      (if (not (boundp 'TeX-master))
          (progn
            (message "Variable TeX-master not set, trying basename(.dvi)")
            (setq TM (jjj-base-name (cur-file)))
            )
        (progn
          (if (eq TeX-master nil)
              (progn
                (message "Variable TeX-master is nil, trying basename(.dvi)")
                (setq TM (jjj-base-name (cur-file)))
                )
            (setq TM TeX-master)
            ) ) ) )

    (if (boundp 'MyRelDir) ;; can set to "" with LaTeX3
;;        (setq frel (concat MyRelDir "/" fname)) ;; LaTeX2
        (setq fname (concat "./" fname)) ;; LaTeX3
      )

    (setq TM (concat TM ".dvi"))

    (message "TM=[%s] cmd=[%s] fname=[%s] sline=[%s]" TM cmd fname sline);; for debug
    (if (not (file-exists-p TM))
        (error (concat "No dvi file (" TM ")"))
      ; else launch xdvi
      (call-process cmd ; PROGRAM
                    nil ; INFILE
                    0   ; BUFFER
                    nil ; DISPLAY
                    ; rest: ARGS
                    TM ;; full name of dvi file (usually==TeX-master)
;;                    "-geom" "880x1100-0-0"
                    "-geom" "997x1498--70--200"
;;                    "-expert" "-s" "6"
                    "-expert" "-s" "5"
                    "-gamma" "1.8"
                    "-sourceposition" (concat sline " " fname)
                    )
      )
    )
  )


;; ------ start special stuff for fxtbook ------
;; bound to keys in hook for tex-mode

(defun jjj-tex-insert-listing ()
  "Insert listing env (for fxtbook)"
  (interactive)
  (progn
    (insert
     "%\n"
     "{\\codesize\n"
     "\\begin{listing}{1}\n"
     "\n"
     "\\end{listing}\n"
     "}%\n"
     "%\n")
    (previous-line 4) ) )

(defun jjj-tex-insert-code ()
  "Insert verbatim env (for fxtbook)"
  (interactive)
  (progn
    (insert
     "%\n"
     "{\\codesize\n"
     "\\begin{verbatim}\n"
     "\n"
     "\\end{verbatim}\n"
     "}%\n"
     "%\n")
    (previous-line 4) ) )

(defun jjj-tex-insert-eqn ()
  "Insert eqnarray (for fxtbook)"
  (interactive)
  (progn
    (insert
     "%\n"
     "\\begin{eqnarray}\n"
     "\n"
     "\\end{eqnarray}\n"
     "%\n")
    (previous-line 3) ) )

(defun jjj-tex-insert-subeqn ()
  "Insert sub-eqnarray (for fxtbook)"
  (interactive)
  (progn
    (insert
     "%\n"
     "\\begin{subequations}\n"
     "\\begin{eqnarray}\n"
     "\n"
     "\\end{eqnarray}\n"
     "\\end{subequations}\n"
     "%\n")
    (previous-line 4) ) )

(defun jjj-tex-insert-figure ()
  "Insert figure (for fxtbook)"
  (interactive)
  (progn
    (insert
     "%%%%%%%%%%%%%%%%%%%\n"
     "%\n"
     "\\begin{figure}%[hbt]\n"
     "%\n"
     "\n"
     "%\n"
     "\\caption{%\\jjlabel{fig:}\n INSERTCAPTIONTEXT\n"
     "}\n"
     "\\end{figure}\n"
     "%\n"
     "%%%%%%%%%%%%%%%%%%%\n")
    (previous-line 8) ) )


(defun jjj-tex-insert-bibitem ()
  "Insert bibitem (for fxtbook)"
  (interactive)
  (progn
    (insert
     "%%: NIL\n"
;;     "\\jjbibitem{}{:\n"
     "\\bibitem{}{:\n"
     "\\jjbibtitle{},\n"
     ", \\bdate{}.\n"
     "URL: \\url{}.}\n"
     "\\jjfile{nil.pdf}\n")
    (previous-line 5)
    (beginning-of-line)
    (forward-char 13) ) )

(defun jjj-tex-insert-bibitem-b ()
  "Insert bibitem with extra blank lines (for fxtbook)"
  (interactive)
  (progn
    (insert "\n\n")
    (previous-line 1)
    (jjj-tex-insert-bibitem)
    )
  )

;; ------  end special stuff for fxtbook ------

