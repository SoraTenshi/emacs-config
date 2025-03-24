;;; modal.el ---
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

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "RET") 'org-agenda-switch-to)


;;; modal.el ends here
