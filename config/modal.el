;;; evil.el --- Evil mode configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; evil mode configuration
;;; Code:

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(setq evil-cross-lines t
      evil-move-beyond-eol t)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(evil-set-initial-state 'org-agenda-mode 'motion)

;;; evil.el ends here
