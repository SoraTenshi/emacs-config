;;; rust.el ---
;; Setting up emacs for the rust programming language
;;; Code:

(use-package rust-mode
  :hook (rust-mode . eglot-mode))

;;; rust.el ends here
