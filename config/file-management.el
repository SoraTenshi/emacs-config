;;; file-management.el --- Everything about managing files. -*- lexical-binding: t; -*-
;;; Commentary:
;; Fuzzy finder, whatever...
;;; Code:

(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :init (setq consult-ripgrep-args
              "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --no-ignore-dot --no-require-git"))

(defun nav/global-search (dir)
  "Search a DIR for a matching regex."
  (interactive "DDirectory: ")
  (consult-ripgrep dir))

(defun nav/find-file-rec ()
  "Search a base-dir which either is the project root or the directory of the current buffer."
  (interactive "sFile: ")
  (if (project-current)
      (project-find-file)
    (let* ((files (directory-files-recursively default-directory ".*" t))
           (choice (completing-read "Find file: " files nil t)))
      (when choice (find-file choice)))))

;;; file-management.el ends here
