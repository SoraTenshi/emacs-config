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

(defun manipulation/unsurround ()
  "Unsurrounds the innermost given characters."
  (interactive)
  (let* ((pairs '((?\( . ?\))
                  (?\[ . ?\])
                  (?\{ . ?\})
                  (?\< . ?\>)))
         (open (read-char "Unsurrond with: "))
         (close (or (cdr (assoc open pairs)) open))
         (has-region (use-region-p))
         (beginning (if has-region (region-beginning) (point)))
         (end (if has-region (region-end) (min (1+ (point)) (point-max))))
         open-pos close-pos)
    (save-excursion
      ;; Search backwards
      (if (and (> beginning (point-min)) (eq (char-before beginning) open))
          (setq open-pos (1- beginning))
        (goto-char beginning)
        (when (search-backward (char-to-string open) nil t)
          (setq open-pos (point))))
      ;; Search forwards
      (goto-char end)
      (if (and (< end (point-max)) (eq (char-after end) close))
          (setq close-pos end)
        (goto-char end)
        (when (search-forward (char-to-string close) nil t)
          (setq close-pos (1- (point))))))
    (if (and open-pos close-pos (<= open-pos beginning) (>= close-pos end))
        (progn
          (save-excursion
            (goto-char close-pos)
            (delete-char 1)
            (goto-char open-pos)
            (delete-char 1))
          (message "Removed surrounding %c ... %c" open close))
      (message "No surrounding delimiters matching %c and %c found" open close))))

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
