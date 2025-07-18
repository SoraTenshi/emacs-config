;;; org-mode.el --- Configuration for org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;; Possibly some stuff for org mode? Not sure.
;;; Code:

(make-directory "~/org/roam" t)

(use-package org
  :ensure t
  :config
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-confirm-babel-evaluate nil))

(use-package org-roam
  :ensure t
  :straight t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config (setq org-modern-table nil
                org-modern-hide-stars nil
                org-pretty-entities t))

;; Cheatsheet:
;; %?	Cursor position after insertion (usually goes at the end)
;; %t	Timestamp (<2025-03-24 Mon>)
;; %T	Timestamp with time (<2025-03-24 Mon 14:32>)
;; %u	Inactive timestamp ([2025-03-24 Mon])
;; %U	Inactive timestamp with time ([2025-03-24 Mon 14:32])
;; %i	Initial content (e.g., selected region or from clipboard)
;; %a	Link to the location where org-capture was called
;; %A	Like %a, but also includes surrounding text as context
;; %l	Link only (without description) to where capture was called
;; %F	Full path to the file where capture was called
;; %f	File name (no path) where capture was called
;; %K	Link to current kill ring (top entry)
;; %k	Content of the top of the kill ring
;; %n	User’s full name (user-full-name)
;; %x	Content of the X clipboard (if available)
;; %c	Current clocked-in task (as link)
;; %^G	Prompt for one or more tags (completion-enabled)
;; %^g	Prompt for a single tag (completion-enabled)
;; %^T	Prompt for timestamp (like %T)
;; %^t	Prompt for timestamp (like %t)
;; %^L	Prompt for location (link)
;; %^C	Prompt for a clipboard content
;; %^n	Prompt for a name (string)
;; %^{Prompt}	Custom prompt. E.g., %^{Title} will ask for title
;; %[file]	Insert content of file
;; %(sexp)	Evaluate elisp expression and insert the result

;;; org-mode.el ends here
