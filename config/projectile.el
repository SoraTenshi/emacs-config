;;; projectile.el --- Settings for projectile -*- lexical-binding: t; -*-
;;; Commentary:
;; Projectile setup or something
;;; Code:

(use-package projectile
  :init (setq projectile-project-search-path '("~/dev" "~/source/repos/"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;; projectile.el ends here
