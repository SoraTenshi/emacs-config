;;; lsp.el --- Configuration for LSP -*- lexical-binding: t; -*-
;;; Commentary:
;; This file includes my configuration for Language Servers and their
;; corresponding auto-completion window.
;;; Code:

(use-package eglot
  :hook ((go-mode rust-mode zig-mode) . eglot)
  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))

(add-hook 'prog-mode-hook #'flymake-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls"))))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :diminish corfu-mode
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-delay 5))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package cape
  :straight t)

(setq-local completion-at-point-functions
            (list (cape-capf-buster #'eglot-completion-at-point)
                  #'cape-yasnippet))

(use-package eldoc-box
  :straight t)

(use-package consult-eglot
  :ensure t
  :straight t
  :after (consult eglot))

(defun lsp/corfu-yasnippet-expand ()
  "Expand function for completion hook."
  (when (yas-expand)
    t))
(add-hook 'corfu-after-completion-hook #'lsp/corfu-yasnippet-expand)

(defun nav/next-diagnostic ()
  "Go to next diagnostic."
  (interactive)
  (flymake-goto-next-error))

(defun nav/prev-diagnostic ()
  "Go to previous diagnostic."
  (interactive)
  (flymake-goto-prev-error))

(defun ui/diagnostic-list ()
  "Open a list of all diagnostics."
  (interactive)
  (flymake-show-buffer-diagnostics)
  (switch-to-buffer-other-window
   (seq-find (lambda (buf)
               (string-match-p "\\*Flymake diagnostics" (buffer-name buf)))
             (buffer-list))))

(defun ui/show-popup-doc ()
  "Show the popup for the symbol under cursor."
  (interactive)
  (cond
   ((eq major-mode 'lisp-mode)
    (sly-describe-symbol (sly-symbol-at-point))
    (when (get-buffer "*sly-description*")
      (pop-to-buffer "*sly-description*")))
   ((eq major-mode 'emacs-lisp-mode)
    (describe-symbol (symbol-at-point))
    (when (get-buffer "*Help*")
      (pop-to-buffer "*Help*")))
   ((eglot-current-server)
    (eldoc-doc-buffer)
    (when (get-buffer "*eldoc*")
      (pop-to-buffer "*eldoc*")))
   (t
    (message "No clue about the current buffer..."))))

;;; lsp.el ends here
