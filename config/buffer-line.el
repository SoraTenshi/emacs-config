;;; buffer-line.el ---
;; Basically just a buffer line (top view)
;;; Code:

(use-package centaur-tabs
  :init
  (setq centaur-tabs-style "slant"
	centaur-tabs-set-icons t
	centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode t))


;;; buffer-line.el ends here
