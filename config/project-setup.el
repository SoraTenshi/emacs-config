;;; project-setup.el --- Settings for project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project setup or something
;;; Code:

(require 'project)
(when (eq system-type 'windows-nt)
  (project-remember-projects-under "~/source/repos" nil)
  (project-remember-projects-under "//wsl.localhost/NixOS/home/nightmare/dev" nil))
(project-remember-projects-under "~/dev" nil)

(setq project-vc-extra-root-markers '(".jj"))

;;; project-setup.el ends here
