;;; helix.el --- Description here -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

;;; dependencies
(use-package s :ensure t :straight t)
(use-package dash :ensure t :straight t)
(use-package avy :ensure t :straight t)
(use-package pcre2el :ensure t :straight t)
(use-package paredit :ensure t :straight t)

(use-package hel
  :straight (hel :type git :host github :repo "anuvyklack/hel")
  :custom (inhibit-startup-screen t)
  :init
  (setq hel-normal-state-cursor 'box
        hel-insert-state-cursor 'box
        hel-motion-state-cursor 'box)
  :config (hel-mode))

;;; helix.el ends here
