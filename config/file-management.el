;;; file-management.el ---
;;; Commentary:
;; Fuzzy finder, whatever...
;;; Code:

(use-package ivy
  :diminish
  :config (ivy-mode 1))

(use-package counsel
  :after ivy)

;;; file-management.el ends here
