;;; init.el --- init -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic entry point for my EMACS config
;;; Code:
(require 'package)

;; add melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package util
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; use-package with straight
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

(load-config "config/editor.el")
(load-config "config/modal.el")
(load-config "config/buffer-line.el")
(load-config "config/lsp.el")
(load-config "config/language-mode.el")
(load-config "config/user.el")
(load-config "config/org-mode.el")
(load-config "config/templates.el")
(load-config "config/projectile.el")
(load-config "config/dashboard.el")
(load-config "config/file-management.el")

(load-config "config/theme.el")

;; Of course, not every information has to be seen in the mode line...
(use-package diminish
  :ensure t)
(diminish 'rainbow-mode)
(diminish 'eldoc-mode)
(diminish 'ivy-mode)
(diminish 'format-all-mode)
(diminish 'flycheck-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'projectile-mode)
(diminish 'yas-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ce0aeeb5350a53e85a2a7d4a5bc33cf3952b88fabb84dd2ec5ba4e2fd40053df" "4d0573a42fa8811959d6ceb9a74432d3f601f6f7e3be521ee9ade4a9dc441afe" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(base16-theme centaur-tabs company-box diminish doom-themes elixir-mode evil-collection eyebrowse flycheck format-all geiser go-mode ligature lsp-ui nix-mode org-modern org-tempo rainbow-delimiters rainbow-mode rust-mode sly zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
