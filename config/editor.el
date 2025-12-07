;;; editor.el --- Editor tweaking. -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic configuration / Setup for EMACS
;;; Code:

;; enforce unix-style lf
(add-hook 'before-save-hook (lambda () (set-buffer-file-coding-system 'utf-8-unix)))

;; no more tabs..
(setq-default indent-tabs-mode nil
              tab-width 4)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4)))

;; format all..
(use-package format-all
  :ensure t
  :straight t
  :diminish format-all-mode
  :hook (prog-mode . format-all-mode))

;; setup the font
(set-face-attribute 'default nil
                    :family "Lilex Nerd Font Mono"
                    :height 160)
(set-face-attribute 'italic nil :slant 'italic)

(set-frame-font "Lilex Nerd Font Mono-16" t t)
(use-package ligature
  :ensure t
  :straight t
  :config
  ;; Enable all ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "[]" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "--" "---" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                            ".-" ".=" ".." "..<" "..." "??" "?:" "?=" "?>" "???" "<-" "<--"
                            "<->" "<+" "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<#>" "<%" "<&>"
                            "<@" "<^>" "</" "</>" "<~" "<<-" "<<=" "<<<" "<<" ">>" ">>="
                            ">>-" ">>>" ">-" ">=" ">>=" ">=>" "<=" "=/=" "=<<" "=="
                            "===" "==>" "=>" "=>>" "<=>" "<=<" "<==" "<=>" "<==>" "!!" "_|_"
                            "|||"
                            "||" "~~" "~~>" "~>" "%%"))
  (global-ligature-mode 't))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\│
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-suppress-auto-error t))

(use-package gcmh
  :straight t
  :diminish
  :init (gcmh-mode 1))

(use-package helpful
  :straight t
  :diminish
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package vdiff
  :straight t
  :commands (vdiff-files vdiff-buffers vdiff-merge-conflict)
  :config (setq vdiff-auto-refine t))

;;; editor.el ends here
