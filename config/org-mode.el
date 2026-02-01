;;; org-mode.el --- Configuration for org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;; Possibly some stuff for org mode? Not sure.
;;; Code:

(make-directory "~/org/roam" t)

(use-package org
  :straight `(org :type built-in)
  :config
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "inbox.org" org-directory)

        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-startup-with-inline-images t
        org-startup-folded t
        org-image-actual-width '(400)

        org-return-follows-link t
        org-mouse-1-follows-link t

        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-cycle-separator-lines 0
        org-ellipsis ""))

(use-package org-roam
  :straight `(org-roam :source melpa)
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
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

(use-package org-modern
  :after org-roam
  :straight t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config (setq org-modern-tag nil
                org-modern-priority nil
                org-modern-todo nil
                org-modern-table nil))

(use-package org-superstar
  :ensure t
  :straight t
  :after 'org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "◆" "◇" "▶" "▷"))
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t))

(custom-set-faces
 '(org-level-1 ((t (:height 1.3 :weight bold :foreground "#7aa2f7"))))
 '(org-level-2 ((t (:height 1.2 :weight bold :foreground "#bb9af7"))))
 '(org-level-3 ((t (:height 1.2 :weight bold :foreground "#7dcfff"))))
 '(org-level-4 ((t (:height 1.1 :weight bold :foreground "#9ece6a"))))
 '(org-level-5 ((t (:height 1.0 :weight bold :foreground "#e0af68"))))
 '(org-level-6 ((t (:height 1.0 :weight bold :foreground "#f7768e"))))
 '(org-level-7 ((t (:height 1.0 :weight bold :foreground "#73daca"))))
 '(org-level-8 ((t (:height 1.0 :weight bold :foreground "#c0caf5"))))

 '(org-document-title ((t (:height 1.3 :weight bold :foreground "#7aa2f7"))))

 '(org-todo ((t (:weight bold :foreground "#f7768e"))))
 '(org-done ((t (:weight bold :foreground "#9ece6a"))))

 '(org-link ((t (:foreground "#7aa2f7" :underline t))))

 '(org-bold ((t (:weight bold :foreground "#c0caf5"))))
 '(org-italic ((t (:slant italic :foreground "#9d7cd8"))))
 '(org-code ((t (:background "#1f2335" :foreground "#e0af68"))))
 '(org-verbatim ((t (:background "#1f2335" :foreground "#73daca"))))

 '(org-block ((t (:background "#1f2335" :extend t))))
 '(org-block-begin-line ((t (:background "#24283b" :foreground "#565f89" :italic t))))
 '(org-block-end-line ((t (:background "#24283b" :foreground "#565f89" :italic t))))

 '(org-special-keyword ((t (:foreground "#565f89"))))
 '(org-drawer ((t (:foreground "#565f89"))))
 '(org-date ((t (:foreground "#bb9af7" :underline t))))

 '(org-table ((t (:foreground "#c0caf5"))))

 '(org-agenda-structure ((t (:weight bold :foreground "#7aa2f7" :height 1.2))))
 '(org-agenda-date ((t (:foreground "#bb9af7" :height 1.1))))
 '(org-agenda-date-today ((t (:foreground "#f7768e" :weight bold :height 1.1))))
 '(org-agenda-date-weekend ((t (:foreground "#565f89" :height 1.1)))))


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
