;;; org-mode.el --- Configuration for org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;; Possibly some stuff for org mode? Not sure.
;;; Code:
(require 'org)

(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files '("~/org/todo.org" "~/org/notes.org"))

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
(setq org-capture-templates '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                               "* TODO %?\n %i\n %a")
                              ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
                               "* %^{Note Title}\n[%U]\n %i\n%?")))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)

;;; org-mode.el ends here
