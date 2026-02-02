;;; exwm.el --- EXWM window manager configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; EXWM configuration
;;; Code:

(defun exwm/set-screen-size ()
  "Set frame to location and size required by my monitor."
  (interactive)
  (let* ((geometry (frame-monitor-attribute 'geometry))
         (width (nth 2 geometry))
         (height (nth 3 geometry)))
    (set-frame-size (selected-frame) width height t)
    (set-frame-position (selected-frame) 0 0)))

(setq frame-resize-pixelwise t)
(setq default-frame-alist
      '((undecorated . t)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (menu-bar-lines . nil)
        (tool-bar-lines . nil)))

(use-package xelb
  :ensure t)

(use-package exwm
  :ensure t)

(require 'exwm)
(setq exwm-workspace-number 10)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(setq exwm-input-global-keys
      `(
        ([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-W] . exwm-workspace-move-window)
        ([?\s-p] . exwm/run-app)
        ([?\s-P] . exwm/power-menu)
        ([?\s-s] . browse/do-browse)
        ([?\s-h] . windmove-left)
        ([?\s-j] . windmove-down)
        ([?\s-k] . windmove-up)
        ([?\s-l] . windmove-right)
        ([?\s-q] . kill-current-buffer)
        ([?\s-f] . exwm-layout-toggle-fullscreen)
        ([?\s-t] . exwm-floating-toggle-floating)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\C-w] . [C-x])
        ([?\M-w] . [C-c])
        ([?\C-y] . [C-v])
        ([?\C-s] . [C-f])))

(exwm-systemtray-mode 1)
(exwm-randr-mode 1)
(exwm-wm-mode 1)

(display-battery-mode 1)

(defvar exwm/app-list nil
  "Collected app-list, cache for all the Applications.")

(defun exwm/get-desktop-apps ()
  (unless exwm/app-list
    (setq exwm/app-list
          (let ((apps '()))
            (dolist (dir '("/usr/share/applications"
                           "/usr/local/share/applications"
                           "~/.local/share/applications"))
              (when (file-directory-p dir)
                (dolist (file (directory-files dir t "\\.desktop$"))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (when (re-search-forward "^Name=\\(.+\\)$" nil t)
                      (let ((name (match-string 1))
                            (exec nil))
                        (goto-char (point-min))
                        (when (re-search-forward "^Exec=\\(.+\\)$" nil t)
                          (setq exec (match-string 1))
                          (setq exec (replace-regexp-in-string " %[a-zA-Z]" "" exec))
                          (push (cons name exec) apps))))))))
            (nreverse apps))))
  exwm/app-list)

(defun exwm/run-app ()
  "Run an application from the app-list."
  (interactive)
  (let* ((apps (exwm/get-desktop-apps))
         (app-names (mapcar #'car apps))
         (choice (completing-read "Run: " app-names nil nil))
         (command (cdr (assoc choice apps))))
    (when command
      (start-process-shell-command command nil command))))

(defun exwm/refresh-app-cache ()
  "Refresh the desktop applications cache."
  (interactive)
  (setq exwm/app-list nil)
  (exwm/get-desktop-apps)
  (message "Application cache refreshed (%d apps found)"
           (length exwm/app-list)))

(defun exwm/run-command ()
  (interactive)
  (let ((command (read-shell-command "$ ")))
    (start-process-shell-command command nil command)))

(defun exwm/power-menu ()
  "Some Power menu options."
  (interactive)
  (let ((choice (completing-read "Power: "
                                 '("Shutdown" "Restart" "Hibernate" "Suspend" "Lock" "Logout")
                                 nil t)))
    (pcase choice
      ("Shutdown" (shell-command "sudo shutdown -h now"))
      ("Restart" (shell-command "sudo reboot"))
      ("Hibernate" (shell-command "sudo zzz -Z"))
      ("Suspend" (shell-command "sudo zzz"))
      ("Lock" (start-process "" nil "slock"))
      ("Logout" (kill-emacs)))))

(setq display-time-string-forms
      '((let ((days ["日" "月" "火" "水" "木" "金" "土"]))
          (format-time-string
           (format "%%m月%%d日(%s)%%H:%%M "
                   (aref days (string-to-number (format-time-string "%w" now))))
           now))))
(display-time-mode 1)

(exwm-input-set-key (kbd "<XF86AudioMute>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")))

(defvar browse/browser "firefox"
  "Browser command for the searches.")

(defconst browse/search-engines
  '(("Search on Kagi"      . "https://kagi.com/search?q=%s")
    ("Search on Crates.io" . "https://crates.io/search?q=%s")
    ("Search on GitHub"    . "https://github.com/search?q=%s")
    ("Search on Wikipedia" . "https://en.wikipedia.org/wiki/Special:Search?search=%s")
    ("Search on YouTube"   . "https://www.youtube.com/results?search_query=%s")
    ("Browse to URL"       . "%s"))
  "Alist of different search thingies.")

(defun browse/do-browse ()
  "Just browses with that specific thing."
  (interactive)
  (let* ((engine (completing-read "" browse/search-engines nil t))
         (query (read-string (format "%s: " engine)))
         (url-template (cdr (assoc engine browse/search-engines)))
         (url (format url-template (url-hexify-string query))))
    (start-process "browser" nil browse/browser url)))

;;; exwm.el ends here
