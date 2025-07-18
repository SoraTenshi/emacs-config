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

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(setq custom-file "~/.emacs.d/emacs-custom-file.el")
(setq ring-bell-function 'ignore)

;; Create directories for autosaves and backups
 (dolist (dir '("backups" "autosaves"))
   (let ((full-path (expand-file-name dir user-emacs-directory)))
     (unless (file-exists-p full-path)
       (make-directory full-path t))))

;; Redirect #file# .file~ to the location at home
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "autosaves/" user-emacs-directory) t)))

(defconst configuration-root
  "The configuration root directory."
  (expand-file-name "."))

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

;; setup load paths
(defun load-config (file)
  "Load a FILE from the base EMACS directory and prints success / failure state."
  (let ((path (expand-file-name file user-emacs-directory)))
    (if (file-exists-p path)
        (progn
          (message "Loading file %s..." path)
          (load-file path))
      (message "Config file %s not found.." path))))

;; (load-config "config/helix.el")
;; (load-config "config/evil.el")
(load-config "config/editing.el")
(load-config "config/project.el")
(load-config "config/editor.el")
;; (load-config "config/buffer-management.el")
(load-config "config/lsp.el")
(load-config "config/language-mode.el")
;; (load-config "config/user.el")
(load-config "config/org-mode.el")
;; (load-config "config/templates.el")
;; (load-config "config/dashboard.el")
(load-config "config/file-management.el")
(load-config "config/discord.el")
;; (load-config "config/magit.el")
(load-config "config/theme.el")

;; now setup all keybinds..
;; (load-config "config/keybinds-evil.el")
;; (load-config "config/keybinds-helix.el")
(load-config "config/keybinds.el")

;; Of course, not every information has to be seen in the mode line...
(use-package diminish
  :ensure t)
(diminish 'eldoc-mode)
(diminish 'ivy-mode)
(diminish 'format-all-mode)
(diminish 'flycheck-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'projectile-mode)
(diminish 'yas-minor-mode)
(diminish 'whitespace-mode)
(diminish 'which-key-mode)
(diminish 'better-jumper-local-mode)
(diminish 'helix-normal-mode)
(diminish 'helix-insert-mode)

;;; init.el ends here
