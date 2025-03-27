;;; modal.el --- Modal editing configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; Modal editing configuration (generic, because subject to change)
;;; Code:

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(evil-set-initial-state 'org-agenda-mode 'motion)

(setq evil-undo-system 'undo-redo)

(evil-define-key 'normal 'global (kbd "C-u") #'evil-scroll-up)
(evil-define-key 'visual 'global (kbd "C-u") #'evil-scroll-up)
(evil-define-key 'motion 'global (kbd "RET") #'org-agenda-switch-to)

;;; modal.el ends here
