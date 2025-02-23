;;; init.el --- init
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
(setq use-package-always-ensure t)

;; setup load paths
(defun load-config (file)
  "Load a FILE from the base EMACS directory and prints success / failure state."
  (let ((path (expand-file-name file user-emacs-directory)))
    (if (file-exists-p path)
	(progn
	  (message "Loading file %s..." path)
	  (load-file path))
      (message "Config file %s not found.." path))))

(use-package flycheck
  :init (global-flycheck-mode))

(load-config "config/editor.el")
(load-config "config/modal.el")
(load-config "config/theme.el")
(load-config "config/buffer-line.el")
(load-config "config/lsp.el")
(load-config "config/language-mode.el")
(load-config "config/user.el")

;; Of course, not every information has to be seen in the mode line...
(use-package diminish)
(diminish 'rainbow-mode)
(diminish 'eldoc-mode)
(diminish 'ivy-mode)
(diminish 'format-all-mode)
(diminish 'flycheck-mode)
(diminish 'company-box-mode)
(diminish 'company-mode)
(diminish 'evil-collection-unimpaired-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(nix-mode company-box company lsp-ui flycheck diminish counsel ivy zig-mode geiser rust-mode jai-mode go-mode elixir-mode sly delight centaur-tabs doom-themes tokyo-night-theme rainbow-mode rainbow-delimiters evil ligature)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
