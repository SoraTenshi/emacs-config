;;; buffer-management.el --- Configuration of buffers -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically just a buffer line (top view)
;;; Code:

(use-package all-the-icons
  :ensure t)

(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-plain-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'under
        centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match))

(defun buffer/centaur-tabs-all-groups ()
  "List all centaur-tab groups."
  (let* ((raw (delete-dups
               (apply #'append
                      (mapcar (lambda (buf)
                                (with-current-buffer buf
                                  (funcall centaur-tabs-buffer-groups-function)))
                              (buffer-list)))))
         (blacklist '("Common")))
    (seq-remove (lambda (g) (member g blacklist)) raw)))

(defun buffer/centaur-switch-to-group ()
  "Open a minibuffer to select a buffer group."
  (interactive)
  (let* ((groups (buffer/centaur-tabs-all-groups))
         (g (completing-read "Group: " groups nil t)))
    (when (and g (not (string-empty-p g)))
      (let ((b (seq-find
                (lambda (buf)
                  (with-current-buffer buf
                    (member g (funcall centaur-tabs-buffer-groups-function))))
                (buffer-list))))
        (when b
          (switch-to-buffer b)
          (centaur-tabs-update))
        (message "Switched to group: %s" g)))))

(defun buffer/centaur-tabs-remove-group (group)
  "Kill all buffers in the centaur-tabs GROUP, then update tabs."
  (interactive
   (list (completing-read
          "Remove group: "
          (buffer/centaur-tabs-all-groups)
          nil t)))
  (dolist (buf (buffer/centaur-tabs-all-groups))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (centaur-tabs-delete-tab buf)
  (message "Removed group %s" group))

;;; buffer-management.el ends here
