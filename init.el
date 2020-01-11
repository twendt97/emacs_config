

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/jjemacs/main.el")

;enable yaml-mode
;(load "~/.emacs.d/yaml-mode/yaml-mode.el")
;(require 'yaml-mode)
;(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(load "./sql-upcase.el")
(when (require 'sql-upcase nil :noerror)
  (add-hook 'sql-mode-hook 'sql-upcase-mode)
  (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))


;; end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-check-signature nil)
 '(package-selected-packages (quote (auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(subscript ((default) (nil nil)))
 '(superscript ((default) (nil nil)))
 '(tex-verbatim ((default) (nil nil))))

;; deactivate backup files
(setq make-backup-files nil)

