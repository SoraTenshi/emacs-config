;;; early-init.el --- early entry into EMACS -*- lexical-binding: t; -*-
;;; Commentary:
;; Early Entry ^_^
;;; Code:

(message "Loading early-init.el...")
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (split-window-right)
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

(setq frame-inhibit-implied-resize t)
(when (getenv "EXWM")
      (setq frame-inhibit-implied-resize nil))
(setq package-enable-at-startup nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(message "Done with early-init.el")

;;; early-init.el ends here
