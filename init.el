;;; init.el --- init -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic entry point for my EMACS config
;;; Code:
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil)
(blink-cursor-mode 0)

(let ((alpha 98))
  (set-frame-parameter (selected-frame) 'alpha-background alpha)
  (add-to-list 'default-frame-alist `(alpha-background . ,alpha)))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq custom-file "~/.emacs.d/emacs-custom-file.el")
(setq ring-bell-function 'ignore)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-wheel-progressive-speed nil)
(setq scroll-preserve-screen-position t)

(setq-default display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Redirect #file# .file~ to the location at home
(custom-set-variables
 '(auto-save-file-name-transform '((".*" "~/.emacs.d/autosaves/\\l" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

(make-directory "~/.emacs.d/autosaves/" t)

(defconst configuration-root
  (expand-file-name ".")
   "The configuration root directory.")

;; straight for github stuff (basically only the theme for now...
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-built-in-package-overrides '((flymake . "30.1")))

;; setup load paths
(defun load-config (file)
  "Load a FILE from the base EMACS directory and prints success / failure state."
  (let ((path (expand-file-name file user-emacs-directory)))
    (if (file-exists-p path)
        (progn
          (message "Loading file %s..." path)
          (load-file path))
      (message "Config file %s not found.." path))))

(use-package diminish
  :ensure t
  :straight t)

(load-config "config/org-mode.el")
(load-config "config/editing.el")
(load-config "config/editor.el")
(load-config "config/lsp.el")
(load-config "config/project-setup.el")
(load-config "config/language-mode.el")
(load-config "config/file-management.el")
(load-config "config/discord.el")
(load-config "config/magit.el")
(load-config "config/theme.el")

;; now setup all keybinds..
(load-config "config/keybinds.el")

;;; init.el ends here
(put 'upcase-region 'disabled nil)
