;;; editing.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(use-package surround
  :ensure t
  :straight t
  :bind-keymap ("C-c m" . surround-keymap))

(defun quit/safe-exit ()
"Delete this window, but don't ever kill EMACS."
(interactive)
(if (one-window-p)
    (message "Saved your startup time ;)")
  (delete-window)))

;;; editing.el ends here
