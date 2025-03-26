;;; dashboard.el --- Dashboard configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; My dashboard i guess.
;;; Code:

(use-package dashboard
  :init (setq initial-buffer-choice #'dashboard-open)
  :config (setq dashboard-banner-logo-title "こんにちは～"
                dashboard-startup-banner 2
                dashboard-center-content t
                dashboard-vertically-center-content t
                dashboard-navigation-cycle t)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-icon-type 'all-the-icons))

;;; dashboard.el ends here
