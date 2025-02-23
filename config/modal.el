;;; modal.el ---
;; Modal editing configuration (generic, because subject to change)
;;; Code:

(use-package evil
  :init (setq evil-want-integration t
	      evil-want-keybinding nil)
  :config (evil-mode 1))

;;; modal.el ends here
