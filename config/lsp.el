;;; lsp.el --- Configuration for LSP -*- lexical-binding: t; -*-
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

(straight-use-package 'eldoc-box)

(defun nav/next-diagnostic ()
  (interactive)
  (cond
   ((and (bound-and-true-p lsp-mode)
         (fboundp 'lsp-next-diagnostic))
    (lsp-next-diagnostic))
   ((fboundp 'lsp-ui-flycheck-next)
    (lsp-ui-flycheck-next))
   ((fboundp 'flycheck-next-error)
    (flycheck-next-error))
   ((fboundp 'flymake-goto-next-error)
    (flymake-goto-next-error))
   (t
    (message "No diagnostic navigation function available."))))

(defun nav/previous-diagnostic ()
  (interactive)
  (cond
   ((and (bound-and-true-p lsp-mode)
         (fboundp 'lsp-previous-diagnostic))
    (lsp-previous-diagnostic))
   ((fboundp 'lsp-ui-flycheck-previous)
    (lsp-ui-flycheck-previous))
   ((fboundp 'flycheck-previous-error)
    (flycheck-previous-error))
   ((fboundp 'flymake-goto-previous-error)
    (flymake-goto-previous-error))
   (t
    (message "No diagnostic navigation function available."))))

(defun ui/diagnostic-list ()
  (interactive)
  (cond
   ((and (bound-and-true-p lsp-mode)
         (fboundp 'lsp-treemacs-errors-list))
    (lsp-treemacs-errors-list))
   ((and (bound-and-true-p lsp-mode)
         (fboundp 'lsp-ui-flycheck-list))
    (lsp-ui-flycheck-list))
   ((bound-and-true-p flycheck-mode)
    (flycheck-list-errors))
   ((bound-and-true-p flymake-mode)
    (flymake-show-diagnostics-buffer))
   (t
    (message "No diagnostics list viewer available."))))

(defun ui/show-popup-doc ()
  "Show the popup for the symbol under cursor."
  (interactive)
  (let ((sym (symbol-at-point)))
    (cond
     ((and (bound-and-true-p lsp-mode)
           (fboundp 'lsp-ui-doc-show))
      (lsp-ui-doc-show))
     ((and sym (fboundp 'eldoc-box-help-at-point))
      (eldoc-box-help-at-point))
     ((and sym (fboundp 'describe-symbol))
      (describe-symbol sym))
     (sym (message "No Documentation available for '%s'." sym)))))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "SPC k") #'ui/show-popup-doc
    (kbd "SPC d") #'ui/diagnostic-list
    (kbd "] d") #'nav/next-diagnostic
    (kbd "[ d") #'nav/previous-diagnostic))

;;; lsp.el ends here
