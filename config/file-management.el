;;; file-management.el --- Everything about managing files. -*- lexical-binding: t; -*-
;;; Commentary:
;; Fuzzy finder, whatever...
;;; Code:

(use-package vertico
  :straight t
  :init (vertico-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

(use-package consult
  :straight t
  :init (setq consult-ripgrep-args
              "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --no-ignore-dot --no-require-git"))

(defun nav/global-search (dir)
  "Search a DIR for a matching regex."
  (interactive "DDirectory: ")
  (consult-ripgrep dir))

(defun reload-file ()
  "Reload the current file."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;;; file-management.el ends here
