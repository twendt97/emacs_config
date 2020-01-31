;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(load "~/.emacs.d/jjemacs/main.el")

;; enable window splitting with hydra. See https://github.com/abo-abo/hydra/wiki/Window-Management
;(load "~/.emacs.d/custom_packages/window_management.el")

;enable yaml-mode
(load "~/.emacs.d/yaml-mode/yaml-mode.el")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;(load "./sql-upcase.el")
;(when (require 'sql-upcase nil :noerror)
;  (add-hook 'sql-mode-hook 'sql-upcase-mode)
;  (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))


;; end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(package-check-signature (quote allow-unsigned))
 '(package-selected-packages
   (quote
    (irony-eldoc company-irony company ace-window auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(subscript ((default) (nil nil)))
 '(superscript ((default) (nil nil)))
 '(tex-verbatim ((default) (nil nil))))

;; Set dictionary to german
 (setq ispell-dictionary "german")

;; deactivate backup files
(setq make-backup-files nil)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-unsigned-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(use-package company
             :ensure t
             :config
             (setq company-idle-delay 0)
             (setq company-minimum-prefix-length 2))


(use-package company-irony
             :ensure t
             :config
             (require 'company)
             (add-to-list 'company-backends 'company-irony))

(use-package irony
             :ensure t
             :config
             (add-hook 'c++-mode-hook 'irony-mode)
             (add-hook 'c-mode-hook 'irony-mode)
             (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;(add-hook 'after-init-hook 'global-company-mode)
