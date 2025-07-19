;;; keybinds.el --- Keybinds -*- lexical-binding: t; -*-
;;; Commentary:
;; Keybindings for vanilla EMACS
;;; Code:

(add-hook 'before-save-hook #'delete-trailing-whitespace)




(global-set-key (kbd "C-c k") 'ui/show-popup-doc)
(global-set-key (kbd "C-c d") 'ui/diagnostic-list)
(global-set-key (kbd "C-c s") 'nav/global-search)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c a") 'eglot-code-actions)
(global-set-key (kbd "C-c p") 'project/switch)
(global-set-key (kbd "C-.") 'set-mark-command)

(provide 'keybinds)
;;; keybinds.el ends here
