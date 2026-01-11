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
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 1)
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert)
              ([return]  . corfu-insert)))

(use-package cape
  :straight t
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

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

(add-hook 'display-buffer-alist
          '("\\*Help\\*"
            (display-buffer-reuse-window display-buffer-in-side-window)
            (side . bottom)
            (slot . 0)
            (window-height . 10)))

(defun ui/show-popup-doc ()
  "Show documentation popup for the symbol under cursor."
  (interactive)
  (let ((buf nil))
    (cond
     ((derived-mode-p 'lisp-mode)
      (when (fboundp 'sly-describe-symbol)
        (sly-describe-symbol (sly-symbol-at-point))
        (setq buf (get-buffer "*sly-description*"))))
     ((derived-mode-p 'emacs-lisp-mode)
      (when-let ((sym (symbol-at-point)))
        (describe-symbol sym)
        (setq buf (get-buffer "*Help*"))))
     ((and (boundp 'eglot--managed-mode)
           eglot--managed-mode
           (eglot-current-server))
      (eldoc-doc-buffer)
      (setq buf (get-buffer "*eldoc*")))
     (t
      (message "No documentation available for this buffer.")))
    (when (buffer-live-p buf)
      (display-buffer
       buf
       '((display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-height . 10))))))

;;; lsp.el ends here
