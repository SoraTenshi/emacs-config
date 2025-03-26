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
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point))

;; Setup Company mode
(use-package company
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun ui/show-popup-doc ()
  "Show the popup for the symbol under cursor."
  (interactive)
  (let ((sym (symbol-at-point)))
  (cond
   ((and (bound-and-true-p lsp-mode)
         (fboundp 'lsp-ui-doc-show))
    (lsp-ui-doc-show))

   ((and sym (fboundp 'eldoc-message)
         (fboundp 'symbol-function))
    (eldoc-message (documentation (symbol-function sym))))

   ((and sym (fboundp 'describe-symbol))
    (describe-symbol sym))

   (sym (message "No Documentation available for '%s'." sym)))))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC k") #'ui/show-popup-doc))

;;; lsp.el ends here
