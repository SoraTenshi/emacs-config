;;; project.el --- Settings for project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project setup or something
;;; Code:

(setq project-search-path '(("~/dev" . 2)
                            ("~/source/repos" . 2)))

(use-package consult-project-extra
  :straight t
  :ensure t)

;; Sets up jj to also be included
(add-to-list 'project-vc-backend-markers-alist '(jj . ".jj"))
(setq project-vc-extra-root-markers '(".jj"))

;;; project.el ends here
