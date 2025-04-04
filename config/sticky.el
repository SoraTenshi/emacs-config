;;; sticky.el --- Sticky context -*- lexical-binding: t; -*-
;;; Commentary:
;; Sticky context for EMACS?
;;; Code:

;; (use-package semantic
;;   :ensure nil
;;   :config
;;   (semantic-mode 1)
;;   (global-semantic-stickyfunc-mode 1))

;; (use-package stickyfunc-enhance
;;   :ensure t
;;   :straight t)

;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(straight-use-package
 '(window-stool :type git :host github :repo "jaszhe/window-stool"))

(add-hook 'prog-mode-hook #'window-stool-mode)

;;; sticky.el ends here
