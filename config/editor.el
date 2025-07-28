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

(add-to-list 'default-frame-alist '(width . 157))
(add-to-list 'default-frame-alist '(height . 37))

(set-frame-font "Lilex Nerd Font Mono-15" nil t)
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

;;; editor.el ends here
