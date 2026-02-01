;;; scheme.el --- Setup for Scheme
;;; Commentary:
;; Setting up EMACS for the scheme programming language
;;; Code:

(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode) . aggressive-indent-mode))

(use-package geiser
  :config
  (setq geiser-active-implementations '(guile racket)))

;;; scheme.el ends here
