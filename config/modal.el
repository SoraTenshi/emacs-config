;;; modal.el --- Modal editing configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; Modal editing configuration (generic, because subject to change)
;;; Code:

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(setq evil-cross-lines t
      evil-move-beyond-eol t)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(evil-set-initial-state 'org-agenda-mode 'motion)

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
  (if (one-window-p t)
      (message "Saved your startup time ;)")
    (evil-window-delete)))

(with-eval-after-load 'evil
  (define-key evil-window-map (kbd "q") #'quit/safe-exit)
  (evil-define-key 'normal 'global (kbd "SPC r") #'clipboard/replace-region)
  (evil-define-key '(normal visual) 'global (kbd "C-u") #'evil-scroll-up)
  (evil-define-key 'motion 'global (kbd "RET") #'org-agenda-switch-to)
  (evil-define-key '(normal visual) 'global (kbd "C-o") #'evil-jump-backward)
  (evil-define-key '(normal visual) 'global (kbd "C-i") #'evil-jump-forward)

  (evil-define-key '(normal visual) 'global (kbd "m") (make-sparse-keymap))
  (evil-define-key '(normal visual) 'global (kbd "m d") #'manipulation/unsurround)
  (evil-define-key '(normal visual) 'global (kbd "m s") #'manipulation/surround)
  (evil-define-key '(normal visual) 'global (kbd "m m") #'evil-jump-item)
  (evil-define-key 'normal 'global (kbd "=") #'format-all-buffer)
  (evil-define-key '(normal visual) 'global (kbd "C-c") #'comment/toggle)
  (evil-define-key 'normal 'global (kbd "SPC c") #'compile)
  (evil-define-key 'normal 'global (kbd "%") #'select/whole-buffer))

;;; modal.el ends here
