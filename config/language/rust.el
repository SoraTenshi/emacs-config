;;; rust.el ---
;; Setting up emacs for the rust programming language
;;; Code:

(use-package rust-mode
  :ensure t
  :straight t)

(use-package rustic
  :ensure t
  :straight t
  :after (rust-mode)
  :custom
  (rustic-rustfmt-args "--edition 2024")
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;;; rust.el ends here
