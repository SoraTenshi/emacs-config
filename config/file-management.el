;;; file-management.el ---
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

(with-eval-after-load 'evil
  ;; SPC mode
  (evil-define-key 'normal 'global (kbd "SPC f") #'find-file)
  (evil-define-key 'normal 'global (kbd "SPC b") #'ibuffer)
  (evil-define-key 'normal 'global (kbd "SPC k") #'kill-buffer)

  ;; g mode
  (evil-define-key 'normal 'global (kbd "g n") #'next-buffer)
  (evil-define-key 'normal 'global (kbd "g p") #'previous-buffer))

;;; file-management.el ends here
