;;; editing.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(defun quit/safe-exit ()
"Delete this window, but don't ever kill EMACS."
(interactive)
(if (one-window-p)
    (message "Saved your startup time ;)")
  (delete-window)))

;;; editing.el ends here
