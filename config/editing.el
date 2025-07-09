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

(defun manipulation/jump-item (&optional count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  (interactive "P")
  (cond
   ;; COUNT% jumps to a line COUNT percentage down the file
   (count
    (move-to-column (current-column))
    (goto-char
     (let ((size (- (point-max) (point-min))))
       (+ (point-min)
          (if (> size 80000)
              (* count (/ size 100))
            (/ (* count size) 100))))))
   ((and (looking-at-start-comment-p)
         (let ((pnt (point)))
           (forward-comment 1)
           (or (not (bolp))
               (prog1 nil (goto-char pnt)))))
    (backward-char))
   ((and (not (eolp)) (looking-at-end-comment-p))
    (forward-comment -1))
   ((and
     (memq major-mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
     (require 'hideif nil t)
     (with-no-warnings
       (let* ((hif-else-regexp (concat hif-cpp-prefix "\\(?:else\\|elif[ \t]+\\)"))
              (hif-ifx-else-endif-regexp
               (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp)))
         (cond
          ((save-excursion (beginning-of-line) (or (hif-looking-at-ifX) (hif-looking-at-else)))
           (hif-find-next-relevant)
           (while (hif-looking-at-ifX)
             (hif-ifdef-to-endif)
             (hif-find-next-relevant))
           t)
          ((save-excursion (beginning-of-line) (hif-looking-at-endif))
           (hif-endif-to-ifdef)
           t))))))
   (t
    (let* ((open (point-max))
           (close (point-max))
           (open-pair (ignore-errors
                        (save-excursion
                          ;; consider the character right before eol given that
                          ;; point may be placed there, e.g. in visual state
                          (when (and (eolp) (not (bolp)))
                            (backward-char))
                          (setq open (1- (scan-lists (point) 1 -1)))
                          (when (< open (line-end-position))
                            (goto-char open)
                            (forward-list)
                            (1- (point))))))
           (close-pair (ignore-errors
                         (save-excursion
                           ;; consider the character right before eol given that
                           ;; point may be placed there, e.g. in visual state
                           (when (and (eolp) (not (bolp)))
                             (backward-char))
                           (setq close (1- (scan-lists (point) 1 1)))
                           (when (< close (line-end-position))
                             (goto-char (1+ close))
                             (backward-list)
                             (point))))))
      (cond
       ((not (or open-pair close-pair))
        ;; nothing found, check if we are inside a string
        (let ((pnt (point))
              (bnd (bounds-of-thing-at-point 'string)))
          (if (not (and bnd (< (point) (cdr bnd))))
              ;; no, then we really failed
              (user-error "No matching item found on the current line")
            ;; yes, go to the end of the string and try again
            (let ((endstr (cdr bnd)))
              (when (or (save-excursion
                          (goto-char endstr)
                          (let ((b (bounds-of-thing-at-point 'string)))
                            (and b (< (point) (cdr b))))) ; not at end of string
                        (condition-case nil
                            (progn
                              (goto-char endstr)
                              (jump-item)
                              nil)
                          (error t)))
                ;; failed again, go back to original point
                (goto-char pnt)
                (user-error "No matching item found on the current line"))))))
       ((< open close) (goto-char open-pair))
       (t (goto-char close-pair)))))))

(defun looking-at-start-comment-p ()
  "Check if point is at the start of a comment."
  (let ((face (get-text-property (point) 'face)))
    (and (nth 4 (syntax-ppss))
         (or (eq face 'font-lock-comment-face)
             (and (listp face) (memq 'font-lock-comment-face face))))))

(defun looking-at-end-comment-p ()
  "Check if point is at the end of a comment."
  (let ((face (get-text-property (point) 'face)))
    (and (nth 4 (syntax-ppss))
         (or (eq face 'font-lock-comment-face)
             (and (listp face) (memq 'font-lock-comment-face face))))))

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

(defun manipulation/select-inside (char)
  "Select inside character pair using syntax tables for better nesting support."
  (interactive "cSelect inside character: ")
  (let* ((pairs '((?\( . ?\))
                  (?\[ . ?\])
                  (?\{ . ?\})
                  (?\< . ?\>)))
         (closing-char (or (cdr (assq char pairs)) char))
         (start-pos nil)
         (end-pos nil))
    (save-excursion
      (cond
       ;; For symmetric pairs (quotes)
       ((eq char closing-char)
        (let ((string-bounds (bounds-of-thing-at-point 'string)))
          (when string-bounds
            (setq start-pos (1+ (car string-bounds))
                  end-pos (1- (cdr string-bounds))))))
       ;; For brackets using syntax scanning
       (t
        (condition-case nil
            (let ((current-pos (point)))
              ;; Try to find enclosing sexp
              (up-list -1)
              (when (eq (char-after) char)
                (setq start-pos (1+ (point)))
                (forward-sexp)
                (setq end-pos (1- (point)))
                ;; Check if original position was inside this pair
                (unless (and (< start-pos current-pos) (< current-pos end-pos))
                  (setq start-pos nil end-pos nil))))
          (error nil)))))
    (if (and start-pos end-pos (< start-pos end-pos))
        (progn
          (goto-char start-pos)
          (push-mark end-pos t t)
          (message "Selected text inside %c%c" char closing-char))
      (message "No matching %c found or cursor not inside pair" char))))

(defun select/whole-buffer ()
  "Select the whole buffer."
  (interactive)
  (helix--select-region (point-min) (point-max)))

(defun quit/safe-exit ()
"Delete this window, but don't ever kill EMACS."
(interactive)
(if (one-window-p)
    (message "Saved your startup time ;)")
  (delete-window)))

;;; editing.el ends here
