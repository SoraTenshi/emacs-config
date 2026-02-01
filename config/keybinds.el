;;; keybinds.el --- Keybinds -*- lexical-binding: t; -*-
;;; Commentary:
;; Keybindings for vanilla EMACS
;;; Code:

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(global-set-key (kbd "C-c k") 'ui/show-popup-doc)
(global-set-key (kbd "C-c d") 'ui/diagnostic-list)
(global-set-key (kbd "C-c s") 'nav/global-search)
(global-set-key (kbd "C-c l") 'find-file-at-point)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c a") 'eglot-code-actions)
(global-set-key (kbd "C-c p") 'project-switch-project)
(global-set-key (kbd "C-c f") 'forward-word)
(global-set-key (kbd "C-c b") 'backward-word)
(global-set-key (kbd "C-c g") 'xref-goto-xref)
(global-set-key (kbd "C-c h") 'ff-find-other-file)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-.")   'set-mark-command)

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-c r") #'consult-history)))

(provide 'keybinds)
;;; keybinds.el ends here
