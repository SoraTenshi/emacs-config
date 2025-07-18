;;; theme.el --- Anything visual -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically, the settings for themeing and everything related
;;; Code:

;; Rainbow brackets & indent guides
;; (use-package rainbow-delimiters
;; :hook (prog-mode . rainbow-delimiters-mode))


(global-hl-line-mode 1)
(custom-set-faces
 '(hl-line ((t (:background nil
                :box (:line-width -1 :color "#3b4261"))))))

(global-whitespace-mode 1)
 (defun theme/adjustments ()
   "Some theme adjustments."
   (set-face-attribute 'whitespace-space nil
                       :foreground "#565f89"
                       :background nil)
   (set-face-attribute 'whitespace-tab nil
                       :foreground "#565f89"
                       :background nil)
   (set-face-attribute 'whitespace-newline nil
                       :foreground "#3b4261"
                       :background nil)
   (set-face-attribute 'whitespace-line nil
                       :foreground nil
                       :background nil
                       :weight 'normal)
   (set-face-attribute 'fringe nil
                       :background "#24283b"))

(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "foster-hangdaan/doom-emacs-themes")
  :ensure t
  :config
  (load-theme 'doom-tokyo-night-storm t)
  (advice-add 'load-theme :after (lambda (&rest _) (theme/adjustments))))

(setq whitespace-display-mappings
      '((space-mark 32 [?\u00B7] [46])
        (newline-mark 10 [?\u21B5 10] [?$ 10])
        (tab-mark 9 [187 9] [92 9])))

;;; theme.el ends here
