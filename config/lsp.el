;;; lsp.el --- Configuration for LSP
;;; Commentary:
;; This file includes my configuration for Language Servers and their
;; corresponding auto-completion window.
;;; Code:

(use-package lsp-mode
  :commands lsp
  :hook ((go-mode rust-mode zig-mode) . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-enable t))

;; Setup Company mode
(use-package company
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; lsp.el ends here
