;;; project.el --- Settings for project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project setup or something
;;; Code:

(project-remember-projects-under "~/source/repos" nil)
(project-remember-projects-under "~/dev" nil)

;; Sets up jj to also be included
(setq project-vc-extra-root-markers '(".jj"))

;;; project.el ends here
