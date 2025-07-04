;;; helix.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(use-package multiple-cursors
  :ensure t)

(use-package helix
  :ensure t
  :after multiple-cursors
  :config
  (helix-multiple-cursors-setup)
  (helix-mode))

(use-package better-jumper
  :ensure t
  :after helix
  :config
  (setq better-jumper-max-length 500)
  (helix-define-key 'normal (kbd "C-s") #'better-jumper-set-jump)
  (helix-define-key 'normal (kbd "C-i") #'better-jumper-jump-forward)
  (helix-define-key 'normal (kbd "C-o") #'better-jumper-jump-backward)
  (better-jumper-mode +1))

;;; helix.el ends here
