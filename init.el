;;;; Thilos .emacs file
;; Thilo Wendt
;; 17.03.2020

;; To find out about any mode, type control-h m
;; while in that mode.  For example, to find out
;; about mail mode, enter mail mode and then type
;; control-h m.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;; load packages from jjemacs
(load "~/.emacs.d/jjemacs/main.el")

;;;; Add melpa repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;;; Text mode and Auto Fill mode
;; The next two lines put Emacs into Text mode
;; and Auto Fill mode, and are for writers who
;; want to start writing prose rather than code.
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq colon-double-space t)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;;; Compare windows
(global-set-key "\C-cw" 'compare-windows)

;;; Rebind 'C-x C-b' for 'buffer-menu'
(global-set-key "\C-x\C-b" 'buffer-menu)

;; deactivate backup files
(setq make-backup-files nil)

;;;; enable yaml-mode
(load "~/.emacs.d/yaml-mode/yaml-mode.el")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;;;; enable company
(use-package company
             :ensure t
             :config
             (setq company-idle-delay 0)
             (setq company-minimum-prefix-length 2))

;;; ...for shell
(use-package company-shell
  :ensure t
  :config
  (require 'company)
  (setq company-shell-delete-dublicates t)
  )

;;; ... for C and C++
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

;;; ...for auctex
(require 'company-auctex)
(company-auctex-init)

;;; activate company if one of the following modes are enabled
(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'sh-mode-hook 'company-mode)
  (add-hook 'sh-mode-hook 'company-quickhelp-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-quickhelp-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-shell))

;; deactivate startup screen
(setq inhibit-startup-screen t)

;; set desired language
(setq ispell-dictionary "american")
                                        ;(setq ispell-dictionary "german")

;; increase font size
(set-face-attribute 'default (selected-frame) :height 220)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-auctex use-package irony-eldoc hydra go-mode gnu-elpa-keyring-update dockerfile-mode company-shell company-quickhelp company-irony cl-lib-highlight auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(subscript ((default) (nil nil)))
 '(superscript ((default) (nil nil)))
 '(tex-verbatim ((default) (nil nil))))


