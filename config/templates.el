;;; templates.el --- Short collection of templates -*- lexical-binding: t; -*-
;;; Commentary:
;; Just some templates to make my life easier i suppose.
;;; Code:

(defun create-file-with-elisp-header (filename)
  "Create a simple elisp file with FILENAME that satisfies flycheck."
  (interactive "FCreate new .el file: ")
  (let* ((buffer (find-file filename))
         (basename (file-name-nondirectory filename)))
    (when (= (buffer-size buffer) 0)
    (insert (format ";;; %s --- Description here -*- lexical-binding: t; -*-\n" basename))
    (insert ";;; Commentary:\n;; Blabla.\n")
    (insert ";;; Code:\n")
    (insert (format "\n\n;;; %s ends here\n" basename))
    (save-buffer))))

;;; templates.el ends here
