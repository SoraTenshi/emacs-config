;;; project-setup.el --- Settings for project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project setup or something
;;; Code:

(require 'project)
(when (eq system-type 'windows-nt)
  (project-remember-projects-under "~/source/repos" nil)
  (project-remember-projects-under "//wsl.localhost/NixOS/home/nightmare/dev" nil))
(project-remember-projects-under "~/dev" nil)

(when (eq system-type 'windows-nt)
  (defun project--files-in-directory (dir ignores &optional files)
    "Use fd instead of find for file listing."
    (let* ((default-directory dir)
           (args (append '("-H" "-t" "f" "-0")
                         (mapcan (lambda (i) (list "-E" i)) ignores)
                         (or files '(".")))))
      (split-string
       (shell-command-to-string
        (mapconcat #'shell-quote-argument (cons "fd" args) " "))
       "\0" t))))

(setq project-vc-extra-root-markers '(".jj"))

;;; project-setup.el ends here
