;;; project.el --- Settings for project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project setup or something
;;; Code:

(setq project-search-path '(("~/dev" . 2)
                            ("~/source/repos" . 2)))

;; Sets up jj to also be included
(setq project-vc-extra-root-markers '(".jj"))

(defun project--root-markers ()
  "Append root markers to already existing markers."
  (append (when (boundp 'project-vc-extra-root-markers)
            project-vc-extra-root-markers)
          '(".git" ".hg" ".bzr" ".svn")))

(defun project--is-root-dir-p (dir)
  "Test whether this is the start of a root DIR."
  (cl-some (lambda (m)
             (file-exists-p (expand-file-name m dir)))
           (project--root-markers)))

(defun project--discover-roots ()
  "Discover all possible project roots."
  (let (roots)
    (cl-labels ((scan (dir depth)
                  (when (and (file-directory-p dir)
                             (not (member (file-name-nondirectory dir)
                                          '("." ".."))))
                    (if (project--is-root-dir-p dir)
                        (push dir roots)
                      (when (> depth 0)
                        (dolist (sub (directory-files dir t "^[^.]" t))
                          (scan sub (1- depth))))))))
      (dolist (entry project-search-path)
        (scan (expand-file-name (car entry)) (cdr entry)))
      (delete-dups roots))))

(defun project/switch ()
  "Select between all known project paths."
  (interactive)
  (let* ((scanned (project--discover-roots))
         (known (project-known-project-roots))
         (all (delete-dups (append known scanned)))
         (choice (completing-read
                  "Select project: " all nil t nil nil nil)))
    (when (and choice (not (string-empty-p choice)))
      (dired choice))))

(evil-define-key 'normal 'global (kbd "SPC p") #'project/switch)

;;; project.el ends here
