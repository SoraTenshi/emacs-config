;;; common-lisp.el ---
;; Setting up emacs for the common-lisp programming language
;;; Code:

(use-package sly
  :commands sly
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

;;; common-lisp.el ends here
