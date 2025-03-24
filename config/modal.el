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

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

;;; modal.el ends here
