;;; buffer-management.el --- Configuration of buffers -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically just a buffer line (top view)
;;; Code:

(use-package all-the-icons
  :ensure t)

;; (use-package centaur-tabs
;;   :init
;;   (setq centaur-tabs-set-icons t
;;         centaur-tabs-plain-icons t
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-set-bar 'under
;;         centaur-tabs-cycle-scope 'tabs)
;;   :config
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-headline-match))

(use-package eyebrowse
  :straight t
  :ensure t
  :init
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(global-tab-line-mode 1)

;;; buffer-management.el ends here
