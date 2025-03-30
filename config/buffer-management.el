;;; buffer-management.el --- Configuration of buffers -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically just a buffer line (top view)
;;; Code:

(use-package all-the-icons
  :ensure t)

(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'under
        centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match))

(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*"))

;;; buffer-management.el ends here
