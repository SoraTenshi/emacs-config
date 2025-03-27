;;; dashboard.el --- Dashboard configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; My dashboard i guess.
;;; Code:

(setq inhibit-startup-screen t
      initial-buffer-choice t)

(defun initial/scratch-buffer ()
  "Create a basic scratch buffer, serving as my dashboard."
  (when (string= (buffer-name) "*scratch*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8)
      (insert";;   ")
      (insert "お帰りなさい～\n")
      (insert ";; → Press SPC f to open a file\n")
      (insert ";; → Press SPC b to switch buffers\n")
      (insert ";; → Press SPC w to kill buffers\n")
      (insert "\n"))))

(add-hook 'emacs-startup-hook #'initial/scratch-buffer)

;;; dashboard.el ends here
