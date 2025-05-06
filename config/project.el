;;; project.el --- Settings for project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project setup or something
;;; Code:

(defun project/switch ()
  "Browse through projects & their folder structure."
  (interactive)
  (with-current-buffer (get-buffer-create "*Project exploration*")
    (erase-buffer)
    (let ((f (lambda ()
               (interactive)
               (dired (ffap-file-at-point)))))
      (mapc (lambda (i)
              (insert (propertize i
                                  'face 'link
                                  'mouse-face 'highlight
                                  'help-echo "Mouse-1: Visit this directory"
                                  'keymap (define-keymap "RET" f "<mouse-1>" f))
                      "\n"))
            (project-known-project-roots)))
    (switch-to-buffer (current-buffer))))

(setq project-search-path '(("~/dev" . 2)
                            ("~/source/repos" . 2)))

(use-package consult-project-extra
  :straight t
  :ensure t)

;; Sets up jj to also be included
(add-to-list 'project-vc-backend-markers-alist '(jj . ".jj"))
(setq project-vc-extra-root-markers '(".jj"))

(evil-define-key 'normal 'global (kbd "SPC p") #'project-switch-project)

;;; project.el ends here
