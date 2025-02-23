;;; scheme.el ---
;; Setting up emacs for the scheme programming language
;;; Code:

(use-package geiser
  :config
  (setq geiser-active-implementations '(guile racket)))

;;; scheme.el ends here
