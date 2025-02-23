;;; theme.el ---
;; Basically, the settings for themeing and everything related
;;; Code:

;; Rainbow brackets & indent guides
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (custom-theme-set-faces
   'doom-tokyo-night
   `(default ((t (:background "#24283b"))))
   `(fringe ((t (:background "#24283b"))))))

;;; theme.el ends here
