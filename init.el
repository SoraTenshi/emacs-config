;;; package --- summary
;;; Commentary:
(require 'package)
;;; Code:
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; enable line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; org-mode
(require 'org)
(setq org-hide-leading-stars t
      org-startup-indented t)

;; remove that ugly toolbar on top
(tool-bar-mode -1)
(menu-bar-mode -1)

;; use-package util function (very nice)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; ensure packages are always compiled
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-always-byte-compile t)

;; set font
(set-face-attribute 'default nil
              :family "Lilex Nerd Font Mono"
		    :height 140)

;; evil mode
;; in the future i just plan on using the command bar from evil
;; and to somewhat port over the helix keybinds
;; but i am not sure how or when i wanna do that.
(use-package evil
  :config
  (evil-mode 1))
(require 'evil)

;; all the icons stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit smart-mode-line doom-modeline helm doom-themes centaur-tabs org-bullets all-the-icons flycheck font-utils rainbow-mode rainbow-delimiters highlight-indent-guides evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-moonlight t))

(use-package flycheck
 :init (global-flycheck-mode))

;; definitions for all the languages i *might* use
(defvar *languages*
  ;; form: ('package-name, 'hook, 'config)
  '((sly . (nil . ((setq inferior-lisp-program "sbcl"))))
    (rust-mode . ((rust-mode) . nil))
    (zig-mode . ((zig-mode) . nil))
    (haskell-mode . (nil . nil))
    (elixir-mode . (nil . nil))
    (geiser . (nil . nil))))

(defun setup-langs (languages)
  "Call \='use-package\=' on all the LANGUAGES in the right form."
  (dolist (entry languages)
    (let* ((package (car entry))
	   (hooks (caar (cdr entry)))
	   (configs (cdar (cdr entry))))
      (use-package package
	:hook (prog-mode . (lambda ()
			     (when hooks
			       (let ((mode (car hooks))
				     (hook (cdr hooks)))
				 (when (derived-mode-p mode)
				   (funcall hook))))))
	:config
	(when configs
	  (dolist (cfg configs)
	    (eval cfg)))))))

(setup-langs *languages*)

;; package stuff comes here
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))
(when (fboundp 'rainbow-mode) (rainbow-mode +1))

(use-package nyan-mode
  :hook (prog-mode . nyan-mode))

(use-package elcord
  :hook (prog-mode . elcord-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package which-key
  :config (which-key-mode))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'auto))

(use-package centaur-tabs
  :config
  (setq centaur-tabs-style "chamfer"
        centaur-tabs-set-icons t
        centaur-tabs-set-bar 'under
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-icon-type 'all-the-icons
        centaur-tabs-close-button "x"
	x-underline-at-descent-line t
	centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(use-package helm
  :init
  (helm-mode 1)
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)))

(use-package delight)

(delight 'projectile-mode nil "projectile")
(delight 'helm-mode nil "helm")
(delight 'which-key-mode nil "which-key")
(delight 'eldoc-mode nil "eldoc")
(delight 'flycheck-mode nil "flycheck")

(use-package esup)

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(provide 'init)
;;; init.el ends here
