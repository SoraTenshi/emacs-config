;;; file-management.el ---
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
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

(use-package consult
  :straight t
  :init (setq consult-ripgrep-args
              "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --no-ignore-dot --no-require-git"))

(defun nav/global-search (dir)
  (interactive "DDirectory: ")
  (consult-ripgrep dir))

(with-eval-after-load 'evil
  ;; SPC mode
  (evil-define-key 'normal 'global (kbd "SPC f") #'find-file)
  (evil-define-key 'normal 'global (kbd "SPC b") #'ibuffer)
  (evil-define-key 'normal 'global (kbd "SPC k") #'kill-buffer)
  (evil-define-key 'normal 'global (kbd "SPC /") #'nav/global-search)

  ;; g mode
  (evil-define-key 'normal 'global (kbd "g n") #'next-buffer)
  (evil-define-key 'normal 'global (kbd "g p") #'previous-buffer))

;;; file-management.el ends here
