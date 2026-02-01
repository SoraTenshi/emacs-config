;;; modal.el --- Modal editing configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; Modal editing configuration (generic, because subject to change)
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

;;; modal.el ends here
