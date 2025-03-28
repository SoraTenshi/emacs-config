;;; modal.el --- Modal editing configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; Modal editing configuration (generic, because subject to change)
;;; Code:

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(evil-set-initial-state 'org-agenda-mode 'motion)

(setq evil-undo-system 'undo-redo)

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

(defun replace-region-with-clipboard ()
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

(evil-define-key 'normal 'global (kbd "SPC r") #'replace-region-with-clipboard)
(evil-define-key 'normal 'global (kbd "C-u") #'evil-scroll-up)
(evil-define-key 'visual 'global (kbd "C-u") #'evil-scroll-up)
(evil-define-key 'motion 'global (kbd "RET") #'org-agenda-switch-to)

(evil-define-key '(normal visual) 'global (kbd "m") (make-sparse-keymap))
(evil-define-key '(normal visual) 'global (kbd "m d") #'manipulation/unsurround)
(evil-define-key '(normal visual) 'global (kbd "m s") #'manipulation/surround)
(evil-define-key '(normal visual) 'global (kbd "m m") #'evil-jump-item)
(evil-define-key 'normal 'global (kbd "=") #'format-all-buffer)
(evil-define-key '(normal visual) 'global (kbd "C-c") #'comment/toggle)

;;; modal.el ends here
