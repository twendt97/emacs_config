;;; modes.el --- set modes for various file types

;;----------------------------------------------
;; Joerg Arndt's  emacs startup files
;; ... online at http://www.jjj.de/
;; your feedback is welcome  mailto: arndt (AT) jjj.de
;; version: 2019-January-16 (17:07)
;;----------------------------------------------


;; default mode:
(setq default-major-mode 'text-mode)


(setq auto-mode-alist
      (append
       '(
;;(add-to-list 'auto-mode-alist '(
         ("\\.txt$" . text-mode)
         ("\\.doc$" . text-mode)
         ("\\.c$" .   c-mode)

         ("\\.C$" .   c++-mode)
         ("\\.cc$" .  c++-mode)
         ("\\.cpp$" . c++-mode)
         ("\\.cxx$" . c++-mode)
         ("\\.h$" .   c++-mode)
         ("\\.hh$" .  c++-mode)
         ("\\.hxx$" . c++-mode)

         ("\\.java$" . java-mode)

;;         ("\\.a$" .   asm-mode)
         ("\\.asm$".  asm-mode)
         ("\\.s$" .   asm-mode)

         ("\\.sh$" .   shell-script-mode)
         ("\\.zsh$" .  shell-script-mode)
         ("\\rc$" .    shell-script-mode)
         ("\\.pl$"   . perl-mode)
         ("\\.perl$" . perl-mode)
         ("\\.el$" .   emacs-lisp-mode)
         ("\\.p$" .    pascal-mode)
         ("\\.pas$" .    pascal-mode)

         ("\\.tex$" . latex-mode)
;;         ("\\.tex$" . auctex-mode);; gives preview-buffer

         ("\\.ind$" . latex-mode)
         ("\\.ptx$" . TeX-mode)
         ("\\.aux$" . plain-tex-mode)
         ("\\.bbl$" . plain-tex-mode)
         ("\\.toc$" . plain-tex-mode)
         ("\\.lof$" . plain-tex-mode)
         ("\\.lot$" . plain-tex-mode)
         ("\\.idx$" . plain-tex-mode)
         ("\\.ind$" . plain-tex-mode)
         ("\\.glo$" . plain-tex-mode)

         ("\\.txi$" . Texinfo-mode)
         ("\\.w$"   . cweb-mode)
         ("\\.ch$"  . cweb-mode)
         ("\\.web$" . web-mode)


         ("mutt-" .  mail-mode)
         ("claws-mail" .  mail-mode)

         ("makefile" .  makefile-mode)
         ("Makefile" .  makefile-mode)
         ("\\.am$" .   makefile-mode)
         ("\\.mk$" .   makefile-mode)
         ("\\.mak$" .  makefile-mode)
         ("\\.make$" . makefile-mode)

;;         ("\\.html$"  . html-helper-mode)
;;         ("\\.htm$"   . html-helper-mode)
;;         ("\\.phtml$" . html-helper-mode)
;;         ("\\.php3$"  . html-helper-mode)
         ("\\.html$"  . web-mode)
         ("\\.htm$"   . web-mode)
         ("\\.phtml$" . html-mode)
         ("\\.php3$"  . html-mode)

         ("\\.sgml$" . sgml-mode)
         ("\\.m$" .    octave-mode)

         ("\\.sage$"   . python-mode)

         ("\\.ma$"   . maplev-mode)
;;         ("\\.map$"   . maplev-mode)
;;         ("\\^maple-" . maple-mode)
         ("\\.maple$" . maplev-mode)
         ("\\.mpl$"   . maplev-mode)

         ("\\.mu$" .   mupad-mode)

         ("\\.mc$"   . maxima-mode)

         ("\\.gp$"  . gp-script-mode) ;; pari
         ("\\.gpi$"  . gp-script-mode) ;; pari
;;         ("\\.gp$"  . shell-script-mode)

         ("\\.gap" . gap-mode)

         ("\\.bin$" . hexl-mode)
         ("\\.rb$" . ruby-mode)
         ("\\.py\\'" . python-mode)
;;         ("\\.xml\\'" . nxml-mode) ;; horribly slow
         ("\\.xml\\'" . xml-mode)
         )
       auto-mode-alist
       )
      )

;;(setq auto-mode-alist-flag 1)


;;(provide 'modes)

;;; modes.el ends here
