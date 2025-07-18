;;; keybinds.el --- Keybinds -*- lexical-binding: t; -*-
;;; Commentary:
;; Keybindings for vanilla EMACS
;;; Code:

(global-set-key (kbd "C-c k") 'ui/show-popup-doc)
(global-set-key (kbd "C-c d") 'ui/diagnostic-list)
(global-set-key (kbd "C-c s") 'nav/global-search)

(provide 'keybinds)
;;; keybinds.el ends here
