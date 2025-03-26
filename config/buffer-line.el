;;; buffer-line.el --- Configuration of the buffer line -*- lexical-binding: t; -*-
;;; Commentary:
;; Basically just a buffer line (top view)
;;; Code:

(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'under)
  :config (centaur-tabs-mode t))

;;; buffer-line.el ends here
