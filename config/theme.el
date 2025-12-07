;;; theme.el --- Anything visual -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically, the settings for themeing and everything related
;;; Code:

;; Rainbow brackets & indent guides
;; (use-package rainbow-delimiters
;; :hook (prog-mode . rainbow-delimiters-mode))

(use-package colorful-mode
  :diminish colorful-mode
  :ensure t
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "■")
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(global-hl-line-mode 1)

(setq whitespace-display-mappings
      '((space-mark 32 [?\u00B7] [46])
        (newline-mark 10 [?\u21B5 10] [?$ 10])
        (tab-mark 9 [187 9] [92 9])))


(global-whitespace-mode 1)
(diminish 'whitespace-mode)

;; (use-package doom-themes
;;   :ensure t
;;   :straight (doom-themes :type git :host github :repo "foster-hangdaan/doom-emacs-themes")
;;   :config
;;   (load-theme 'doom-tokyo-night-storm t)
;;   (run-with-timer 0.1 nil 'theme/adjustments))

(add-to-list 'custom-theme-load-path "~/.emacs.d/config/themes")
(load-theme 'sapporo-night-rebuilt t)

;;; theme.el ends here
