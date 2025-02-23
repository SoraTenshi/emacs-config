;;; lsp.el ---
;; Sets up a lsp client
;;; Code:

(use-package lsp-mode
  :commands lsp
  :hook ((go-mode rust-mode zig-mode) . lsp)
  :config (setq lsp-prefer-flymake nil))

;;; lsp.el ends here
