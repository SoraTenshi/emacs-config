;;; editing.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(defun comment/toggle ()
  "Toggle between commenting a line or uncommenting it."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun manipulation/surround ()
  "Surround region with delimiter pair."
  (interactive)
  (let* ((has-region (use-region-p))
         (beginning (if has-region (region-beginning) (point)))
         (end (if has-region (region-end) (min (1+ (point)) (point-max)))))
    (let* ((open (read-char "Surround with: "))
           (pairs '((?\( . ?\))
                    (?\[ . ?\])
                    (?\{ . ?\})
                    (?\< . ?\>)))
           (close (or (cdr (assoc open pairs)) open)))
      (save-excursion
        (goto-char end)
        (insert (string close))
        (goto-char beginning)
        (insert (string open))))))

(defun clipboard/replace-region ()
  "Replace the selected region with the contents of the clipboard."
  (interactive)
  (if (use-region-p)
      (let ((clip (current-kill 0)))
        (if (use-region-p)
            (progn
              (delete-region (region-beginning) (region-end))
              (insert clip))
          (if (< (point) (point-max))
              (progn
                (delete-region (point) (min (point-max) (+ (point) 1)))
                (insert clip))
            (message "No character to replace at end-of-buffer."))))))

(defun select/whole-buffer ()
  "Select the whole buffer."
  (interactive)
  (evil-goto-first-line)
  (evil-visual-line)
  (evil-goto-line))

(defun quit/safe-exit ()
  "Delete this window, but don't ever kill EMACS."
  (interactive)
  (if (one-window-p)
      (message "Saved your startup time ;)")
    (evil-window-delete)))

;;; editing.el ends here
