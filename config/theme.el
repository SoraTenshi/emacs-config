;;; theme.el --- Anything visual -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically, the settings for themeing and everything related
;;; Code:

;; Rainbow brackets & indent guides
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "foster-hangdaan/doom-emacs-themes")
  :config
  (load-theme 'doom-tokyo-night-storm t))

;;; theme.el ends here
