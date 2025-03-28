;;; magit.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(use-package magit
  :straight t
  :ensure t)

(evil-define-key 'normal 'global (kbd "SPC g") 'magit-status)

;;; magit.el ends here
