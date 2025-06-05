;;; keybinds.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "SPC k") #'ui/show-popup-doc
    (kbd "SPC d") #'ui/diagnostic-list
    ;; (kbd "SPC S-d") #'ui/project-diagnostic-list
    (kbd "SPC r") #'eglot-rename
    (kbd "SPC a") #'eglot-code-action
    (kbd "g r") #'xref-find-references
    (kbd "] d") #'nav/next-diagnostic
    (kbd "[ d") #'nav/previous-diagnostic)
  (evil-define-key 'normal 'global (kbd "SPC f") #'find-file)
  (evil-define-key 'normal 'global (kbd "SPC b") #'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "SPC /") #'nav/global-search)
  (evil-define-key 'normal 'global (kbd "SPC w") #'evil-delete-buffer)

  (define-key evil-window-map (kbd "q") #'quit/safe-exit)

  (evil-define-key 'normal 'global (kbd "SPC g") 'magit-status)
  (evil-define-key '(normal visual) 'global (kbd "SPC B") #'buffer/centaur-switch-to-group)
  (evil-define-key '(normal visual) 'global (kbd "SPC W") #'buffer/centaur-tabs-remove-group)
  (evil-define-key 'normal 'global (kbd "SPC p") #'project/switch)

  (evil-define-key 'global 'normal (kbd "g a") #'org-agenda)
  (evil-define-key 'global 'normal (kbd "g c") #'org-capture)
  (evil-define-key 'global 'normal (kbd "g l") #'org-store-link)
  (evil-define-key 'global 'normal (kbd "g s") #'org-switchb)

  ;; g mode
  (evil-define-key 'normal 'global (kbd "g n") #'centaur-tabs-forward)
  (evil-define-key 'normal 'global (kbd "g p") #'centaur-tabs-backward)
  ;; (evil-define-key 'normal 'global (kbd "g b") #'centaur-tabs-buffer-list))

  (evil-define-key 'normal 'global (kbd "SPC r") #'clipboard/replace-region)
  (evil-define-key '(normal visual) 'global (kbd "C-u") #'evil-scroll-up)
  (evil-define-key 'motion 'global (kbd "RET") #'org-agenda-switch-to)
  (evil-define-key '(normal visual) 'global (kbd "C-o") #'evil-jump-backward)
  (evil-define-key '(normal visual) 'global (kbd "C-i") #'evil-jump-forward)

  (evil-define-key '(normal visual) 'global (kbd "m") (make-sparse-keymap))
  (evil-define-key '(normal visual) 'global (kbd "m d") #'manipulation/unsurround)
  (evil-define-key '(normal visual) 'global (kbd "m s") #'manipulation/surround)
  (evil-define-key '(normal visual) 'global (kbd "m m") #'evil-jump-item)
  (evil-define-key 'normal 'global (kbd "=") #'format-all-buffer)
  (evil-define-key '(normal visual) 'global (kbd "SPC s") #'comment/toggle)
  (evil-define-key 'normal 'global (kbd "SPC c") #'compile)
  (evil-define-key 'normal 'global (kbd "%") #'select/whole-buffer))


;;; keybinds.el ends here
