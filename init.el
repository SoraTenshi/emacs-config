;;; init.el --- Consolidated Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Personal Emacs configuration
;;; Code:

;; ========================================================================
;; UI Configuration
;; ========================================================================

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(blink-cursor-mode 0)
(global-hl-line-mode 1)

(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(setq inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil
      ring-bell-function 'ignore
      frame-inhibit-implied-resize t)

(when (getenv "EXWM")
  (setq frame-inhibit-implied-resize nil))

(let ((alpha 98))
  (set-frame-parameter (selected-frame) 'alpha-background alpha)
  (add-to-list 'default-frame-alist `(alpha-background . ,alpha)))

(setq custom-file "~/.emacs.d/emacs-custom-file.el")

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-wheel-progressive-speed nil
      scroll-preserve-screen-position t)

(setq-default make-backup-files nil)
;; i only have one instance of emacs running as daemon, so this is not required
(setq create-lockfiles nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; ========================================================================
;; Package Management
;; ========================================================================

(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(use-package diminish
  :ensure t)

(use-package eldoc
  :ensure t
  :diminish eldoc-mode)

(use-package autorevert
  :ensure t
  :diminish auto-revert-mode)

(add-hook 'before-save-hook (lambda () (set-buffer-file-coding-system 'utf-8-unix)))
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil
              tab-width 4)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4)))

(defconst font-size
  (if (string= system-type "darwin") 18 14)
  "The font size of EMACS.")

(set-face-attribute 'default nil :font "Iosevka Custom" :height (* 10 font-size))
(set-face-attribute 'italic nil :slant 'italic)
(set-frame-font (font-spec :name "Iosevka Custom"
                           :size font-size))

(when (and (eq system-type 'windows-nt)
           (member "Segoe UI Emoji" (font-family-list)))
  (set-fontset-font t '(#x1F600 . #x1F64F) "Segoe UI Emoji" nil 'prepend))

(global-font-lock-mode t)
(add-hook 'prog-mode-hook 'font-lock-mode)

;; ========================================================================
;; Essential Packages
;; ========================================================================

(use-package format-all
  :ensure t
  :defer t
  :diminish format-all-mode
  :hook (prog-mode . format-all-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-suppress-auto-error t))

(use-package gcmh
  :ensure t
  :defer t
  :functions gcmh-mode
  :diminish gcmh-mode
  :init (gcmh-mode 1))

(use-package helpful
  :ensure t
  :defer t
  :diminish
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-files vdiff-buffers vdiff-merge-conflict)
  :config (setq vdiff-auto-refine t))

(use-package vundo
  :ensure t
  :bind (("C-c u" . vundo)))

;; ========================================================================
;; File Management & Navigation
;; ========================================================================

(use-package vertico
  :ensure t
  :functions vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
              ("C-k" . kill-line)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :functions marginalia-mode
  :init (marginalia-mode 1))

(use-package consult
  :ensure t
  :functions consult-ripgrep
  :init
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --no-ignore-dot --no-require-git"))

(use-package embark
  :ensure t
  :bind (("C-," . embark-act)
         ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(defun nav/global-search (dir)
  "Search a DIR for a matching regex."
  (interactive "DDirectory: ")
  (consult-ripgrep dir))

(defun reload-file ()
  "Reload the current file."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; ========================================================================
;; Project Management
;; ========================================================================

(require 'project)

(when (eq system-type 'windows-nt)
  (project-remember-projects-under "~/source/repos" nil)
  (project-remember-projects-under "//wsl.localhost/NixOS/home/nightmare/dev" nil))
(project-remember-projects-under "~/dev" nil)

(when (eq system-type 'windows-nt)
  (defun project--files-in-directory (dir ignores &optional files)
    "Use fd instead of find for file listing."
    (let* ((default-directory dir)
           (args (append '("-H" "-t" "f" "-0")
                         (mapcan (lambda (i) (list "-E" i)) ignores)
                         (or files '(".")))))
      (split-string
       (shell-command-to-string
        (mapconcat #'shell-quote-argument (cons "fd" args) " "))
       "\0" t))))

(setq project-vc-extra-root-markers '(".jj"))

;; ========================================================================
;; LSP & Completion
;; ========================================================================

(use-package eglot
  :defer t
  :functions eglot-current-server eglot-managed-p
  :hook ((go-mode   . eglot-ensure)
         (rust-mode . eglot-ensure)
         (zig-mode  . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls"))))

(use-package corfu
  :ensure t
  :defer t
  :functions global-corfu-mode corfu-popupinfo-mode
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 1)
  :diminish corfu-mode
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert)
              ([return]  . corfu-insert)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.8))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :defer t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package consult-eglot
  :ensure t
  :defer t
  :after (consult eglot))

(use-package devdocs
  :ensure t
  :defer t)

;; ========================================================================
;; Navigation & Diagnostic Functions
;; ========================================================================

;; (use-package flymake
;;   :ensure t
;;   :functions flymake-show-buffer-diagnostics)
;; (add-hook 'prog-mode-hook #'flymake-mode)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package consult-flycheck
  :ensure t
  :after (flycheck consult))

(add-hook 'display-buffer-alist
          '("\\*Help\\*"
            (display-buffer-reuse-window display-buffer-in-side-window)
            (side . bottom)
            (slot . 0)
            (window-height . 10)))

(defun ui/show-popup-doc ()
  "Show documentation popup for the symbol under cursor."
  (interactive)
  (let ((buf nil))
    (cond
     ((derived-mode-p 'emacs-lisp-mode)
      (when-let ((sym (symbol-at-point)))
        (describe-symbol sym)
        (setq buf (get-buffer "*Help*"))))
     ((derived-mode-p 'lisp-mode)
      (when (fboundp 'sly-describe-symbol)
        (sly-describe-symbol (sly-symbol-at-point))
        (setq buf (get-buffer "*sly-description*"))))
     ((and (eglot-managed-p)
           (eglot-current-server))
      (eldoc-doc-buffer)
      (setq buf (get-buffer "*eldoc*")))
     (t
      (message "No documentation available for this buffer.")))
    (when (buffer-live-p buf)
      (let ((win
             (display-buffer
              buf
              '((display-buffer-reuse-window display-buffer-in-side-window)
                (side . bottom)
                (slot . 0)
                (window-height . 10)))))
        (when (window-live-p win)
          (select-window win))))))

(defun ui/show-current-error ()
  "Show the flycheck error at point."
  (interactive)
  (if-let* ((errors (flycheck-overlay-errors-at (point)))
            (err    (car errors))
            (msg    (flycheck-error-message err))
            (level  (flycheck-error-level err))
            (buf    (get-buffer-create "*current-error*")))
      (progn
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert (propertize (symbol-name level)
                              'face (pcase level
                                      ('error   'flycheck-error)
                                      ('warning 'flycheck-warning)
                                      (_        'flycheck-info)))
                  "\n\n"
                  msg)
          (read-only-mode 1))
        (let ((win (display-buffer
                    buf
                    '((display-buffer-reuse-window display-buffer-in-side-window)
                      (side . bottom)
                      (slot . 2)
                      (window-height . 6)))))
          (when (window-live-p win)
            (select-window win))))
    (message "No flycheck error at point.")))

;; ========================================================================
;; Language Modes
;; ========================================================================

(defvar my/c-custom-types
  '("s8" "s16" "s32" "s64"
    "u8" "u16" "u32" "u64"
    "f32" "f64"
    "b8" "b32" "bool"))

(defun my/c-add-types ()
  (font-lock-add-keywords
   nil
   `((,(regexp-opt my/c-custom-types 'words) . font-lock-type-face))))

(use-package simpc-mode
  :vc (:url "https://github.com/rexim/simpc-mode")
  :mode (("\\.c\\'"   . simpc-mode)
         ("\\.h\\'"   . simpc-mode)
         ("\\.hpp\\'" . simpc-mode)
         ("\\.cpp\\'" . simpc-mode)))

(add-hook 'simpc-mode-hook #'my/c-add-types)

(use-package sly
  :defer t
  :functions sly-symbol-at-point
  :commands sly
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t)

(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

(use-package odin-mode
  :ensure t
  :defer t
  :vc (:url "https://github.com/mattt-b/odin-mode"))

(use-package rust-mode
  :ensure t
  :defer t)

(use-package aggressive-indent
  :ensure t
  :defer t
  :hook ((lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode) . aggressive-indent-mode))

(use-package zig-mode
  :ensure t
  :defer t)

;; ========================================================================
;; Magit
;; ========================================================================

(use-package magit
  :ensure t
  :defer t)

;; ========================================================================
;; Language Display Modes
;; ========================================================================

;; This is the weird `LXXX` thingy in the modeline. No need for that
;; as i'm already using line numbers for programming.
;; (And for the rest i don't really need line numbers...)
(line-number-mode -1)

(defun lang/display-modes ()
  "Enable `display-line-numbers-mode` and `whitespace-mode`."
  (whitespace-mode 1)
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(add-hook 'prog-mode-hook 'lang/display-modes)

;; ========================================================================
;; Compile Commands
;; ========================================================================

(use-package fancy-compilation
  :ensure t
  :defer t
  :custom (fancy-compilation-override-colors)
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;; ========================================================================
;; Org Mode & Org Babel
;; ========================================================================

(dolist (dir '("~/org/notes" "~/org/db"))
  (make-directory dir t))

(use-package org
  :defer t
  :config
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "inbox.org" org-directory)

        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-startup-with-inline-images t
        org-startup-folded t
        org-image-actual-width '(400)

        org-return-follows-link t
        org-mouse-1-follows-link t

        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-cycle-separator-lines 0
        org-ellipsis "")

  (with-eval-after-load 'ob
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (lisp       . t)
       (python     . t))))

  (setq org-babel-lisp-eval-fn 'sly-eval)

  (require 'org-tempo)
  (dolist (template '(("cl"   . "src common-lisp")
                      ("el"   . "src emacs-lisp")
                      ("sh"   . "src shell")
                      ("py"   . "src python")))
    (add-to-list 'org-structure-template-alist template)))

(use-package org-modern
  :ensure t
  :defer t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-hide-stars 'leading
        org-modern-tag nil          ; keep off if you prefer
        org-modern-priority nil
        org-modern-todo t           ; enable this
        org-modern-table t))        ; and this

(defvar org-roam-notes
  (file-truename "~/org/notes/")
  "The directory for all kinds of notes.")

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory       org-roam-notes)
  (org-roam-db-location     "~/org/db/org-roam.db")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

(with-eval-after-load 'org
  (defun org-worklog-file ()
    "Return the current year's work log file path."
    (expand-file-name (format "worklog-%s.org" (format-time-string "%Y"))
                      org-roam-notes))

  (defun org-worklog-update-agenda-files ()
    "Update org-agenda-files to include all worklog files."
    (interactive)
    (setq org-agenda-files
          (directory-files org-roam-notes t "^worklog-[0-9]\\{4\\}\\.org$")))

  (org-worklog-update-agenda-files)
  (setq org-capture-templates
        '(("w" "Work Log" plain
           (file+function (lambda () (org-worklog-file)) org-worklog-goto-day)
           "- %?"
           :empty-lines 0
           :unnarrowed t)))

  (defun org-worklog-goto-month ()
    "Go to or create the current month heading."
    (let ((month-heading (format-time-string "* %B %Y")))
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Summary" nil t)
        (insert "* Summary\n\n")
        (goto-char (point-min)))
      (goto-char (point-min))
      (unless (re-search-forward (concat "^" (regexp-quote month-heading)) nil t)
        (goto-char (point-max))
        (insert "\n" month-heading "\n"))
      (re-search-forward (concat "^" (regexp-quote month-heading)) nil t)
      (end-of-line)))

  (defun org-worklog-goto-day ()
    "Go to or create today's heading under the current month."
    (org-worklog-goto-month)
    (let ((day-heading (format-time-string "** %Y-%m-%d %A"))
          (timestamp (format-time-string "<%Y-%m-%d %a>")))
      (forward-line 1)
      (let ((month-end (save-excursion
                         (if (re-search-forward "^\\* " nil t)
                             (line-beginning-position)
                           (point-max)))))
        (if (re-search-forward (concat "^" (regexp-quote day-heading)) month-end t)
            (progn
              (forward-line 1)
              (when (not (looking-at-p "<[0-9]\\{4\\}-"))
                (insert timestamp "\n"))
              (goto-char (line-end-position)))
          (goto-char month-end)
          (insert "\n" day-heading "\n" timestamp)
          (end-of-line)))))

  (defun org-worklog-goto-specific-day ()
    "Navigate to a specific day in the work log."
    (interactive)
    (let* ((date (org-read-date nil t nil "Go to date: "))
           (year (format-time-string "%Y" date))
           (month (format-time-string "%B %Y" date))
           (day (format-time-string "%Y-%m-%d %A" date))
           (file (expand-file-name (format "worklog-%s.org" year) org-roam-notes)))
      (if (file-exists-p file)
          (progn
            (find-file file)
            (goto-char (point-min))
            (if (re-search-forward (concat "^\\*\\* " (regexp-opt (list day))) nil t)
                (progn
                  (org-show-context)
                  (recenter-top-bottom))
              (message "Day %s not found in worklog" day)))
        (message "Worklog file %s does not exist" file))))

  (defun org-worklog-capture ()
    "Capture an entry for the current work day."
    (interactive)
    (org-capture nil "w"))

  (setq org-agenda-custom-commands
        '(("w" . "Work Log Views")
          ("ww" "Last 7 days" agenda ""
           ((org-agenda-span 7)
            (org-agenda-start-day "-7d")))
          ("wm" "This month" agenda ""
           ((org-agenda-span 'month)
            (org-agenda-start-day "1")))
          ("wy" "This year" agenda ""
           ((org-agenda-span 365)
            (org-agenda-start-day "-365d")))
          ("ws" "Search all work logs" search ""))))

;; ========================================================================
;; Hel mode
;; ========================================================================

(use-package s :ensure t)
(use-package dash :ensure t)
(use-package avy :ensure t)
(use-package pcre2el :ensure t)
(use-package paredit :ensure t)

(use-package hel
  :ensure t
  :vc (:url "https://github.com/anuvyklack/hel" :rev "main")
  :custom (inhibit-startup-screen t)
  :init
  (setq hel-normal-state-cursor 'box
        hel-insert-state-cursor 'box
        hel-motion-state-cursor 'box)
  :config (hel-mode 1))

(with-eval-after-load 'hel
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local hel-mode -1)))
  (add-hook 'vundo-mode-hook
            (lambda ()
              (setq-local hel-mode -1)))

  (defun hel--is-full-line-kill-p ()
    "Check if the most recent kill was a full line."
    (let ((text (car kill-ring)))
      (and text
           (string-suffix-p "\n" text))))

  (defun hel-paste-smart ()
    "Paste below current line if yanked text was a full line."
    (interactive)
    (if (hel--is-full-line-kill-p)
        (progn
          (end-of-line)
          (newline)
          (yank)
          (delete-char -1))
      (hel-paste-after nil)))

  (defun hel-paste-smart-above ()
    "Paste above current line if yanked text was a full line."
    (interactive)
    (if (hel--is-full-line-kill-p)
        (progn
          (beginning-of-line)
          (open-line 1)
          (yank)
          (delete-char -1)
          (beginning-of-line))
      (hel-paste-before)))

  (hel-define-command delete-char-under (count)
    "Deletes the character under the cursor."
    :multiple-cursors t
    :merge-selections t
    (interactive "*p")
    (unless (region-active-p)
      (forward-char 1))
    (hel-cut count))

  (hel-define-command change-char-under ()
    "Change char under cursor and enters insert mode."
    :multiple-cursors nil
    (interactive "*")
    (hel-with-each-cursor
      (unless (region-active-p)
        (forward-char 1)))
    (hel-change))

  (hel-keymap-global-set :state 'normal
    "g h" 'hel-beginning-of-line-command
    "g s" 'hel-first-non-blank
    "g e" 'hel-end-of-buffer
    "d"   'delete-char-under
    "c"   'change-char-under
    "C-d" 'scroll-up-command
    "C-u" 'scroll-down-command
    "p"   'hel-paste-smart
    "P"   'hel-paste-smart-above
    "] d" 'flycheck-next-error
    "[ d" 'flycheck-previous-error
    "v"   'hel-extend-selection
    "G"   nil))

;; ========================================================================
;; Additional Utilities
;; ========================================================================

(use-package docker
  :ensure t
  :defer t
  :bind ("C-c D" . docker))

(defun quit/safe-exit ()
  "Delete this window, but don't ever kill EMACS."
  (interactive)
  (if (one-window-p)
      (message "Saved your startup time ;)")
    (delete-window)))

;; ========================================================================
;; IRC (ERC)
;; ========================================================================

(use-package erc
  :ensure t
  :functions erc-current-nick erc-insert-timestamp-left)

(defvar irc-server-alist
  '(("lcolonq" :server "colonq.computer" :port 26697)
    ("libera.chat" :server "irc.libera.chat" :port 6697)
    ("Digital Grove" :server "irc.dgtlgrove.com" :port 6697))
  "A list of IRC servers i use.")

(use-package erc-join
  :custom (erc-autojoin-channels-alist '(("colonq.computer" "#cyberspace"))))

(defun add-irc-server-to-alist ()
  "Add an IRC server to IRC-SERVER-ALIST."
  (interactive)
  (let* ((server-name (read-string "Server name: "))
         (server-address (read-string "Server address: "))
         (server-port (read-number "Server port: ")))
    (push (cons server-name
                (list :server server-address :port server-port))
          irc-server-alist)))

(defun connect-erc-to-server ()
  "Connect to an IRC server from the IRC-SERVER-LIST."
  (interactive)
  (let* ((server-name (completing-read "Server: " (mapcar #'car irc-server-alist)))
         (server-plist (cdr (assoc server-name irc-server-alist)))
         (nick (read-string "Nickname: " "soranotenshi"))
         (tls-input (read-string "Use TLS (default: yes)? " "y"))
         (use-tls (not (string-match-p "^\\s-*n" tls-input))))
    (apply (if use-tls #'erc-tls #'erc)
           (append server-plist `(:nick ,nick)))))

(setq erc-prompt
      (lambda ()
        (format "[%s] [%s] <%s>"
                (format-time-string "%H:%M")
                (buffer-name)
                (erc-current-nick)))
      erc-insert-timestamp-function #'erc-insert-timestamp-left)

(add-hook 'erc-mode-hook
          (lambda ()
            (setq-local buffer-display-table nil
                        show-trailing-whitespace nil)
            (erc-nickbar-mode 1)
            (whitespace-mode -1)
            (show-paren-mode -1)))

(use-package erc-image)

(add-to-list 'erc-modules 'image)
(erc-update-modules)

;; ========================================================================
;; Theme Configuration
;; ========================================================================

(use-package colorful-mode
  :ensure t
  :defer t
  :diminish colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "■")
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(setq whitespace-display-mappings
      '((space-mark 32 [?\u00B7] [46])
        (newline-mark 10 [?\u21B5 10] [?$ 10])
        (tab-mark 9 [187 9] [92 9])))

(global-whitespace-mode 1)
(diminish 'whitespace-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sapporo-night t)

;; ========================================================================
;; Keybindings
;; ========================================================================

;; user mode bindings (C-c)
(global-set-key (kbd "C-c k")   'ui/show-popup-doc)
(global-set-key (kbd "C-c d")   'consult-flycheck)
(global-set-key (kbd "C-c i")   'ui/show-current-error)
(global-set-key (kbd "C-c s")   'nav/global-search)
(global-set-key (kbd "C-c c")   'compile)
(global-set-key (kbd "C-c a")   'eglot-code-actions)
(global-set-key (kbd "C-c p")   'project-switch-project)
(global-set-key (kbd "C-c f")   'project-find-file)
(global-set-key (kbd "C-c g")   'xref-goto-xref)
(global-set-key (kbd "C-c h")   'ff-find-other-file)
(global-set-key (kbd "C-c ;")   'comment-region)
(global-set-key (kbd "C-c :")   'uncomment-region)
(global-set-key (kbd "C-c =")   'align-regexp)
(global-set-key (kbd "C-c w c") 'org-worklog-capture)
(global-set-key (kbd "C-c w W") 'org-worklog-goto-specific-day)
(global-set-key (kbd "C-c w a") 'org-agenda)
;; override C-x b to consult
(global-set-key (kbd "C-x b")   'consult-buffer)
;; windows requires this. *sigh*
(global-set-key (kbd "C-.")     'set-mark-command)
(global-set-key (kbd "C-=")     'format-all-buffer)

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-c r") #'consult-history)))

;; ========================================================================
;; Load EXWM Configuration (if applicable)
;; ========================================================================

(when (string= (system-name) "navi")
  (defun load-config (file)
    "Load a FILE from the base EMACS directory."
    (let ((path (expand-file-name file user-emacs-directory)))
      (if (file-exists-p path)
          (progn
            (message "Loading file %s..." path)
            (load-file path))
        (message "Config file %s not found.." path))))

  (load-config "exwm/exwm.el")
  (load-config "exwm/weather.el"))

(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)

;; ========================================================================
;; Experimental stuff
;; ========================================================================

(use-package mpv :defer t)

;;; init.el ends here
