;;; editor.el --- Editor tweaking. -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic configuration / Setup for EMACS
;;; Code:

;; enable line (relative) numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; remove the "tool" bar on top
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; no more tabs..
(setq-default indent-tabs-mode nil
              tab-width 4)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4)))

;; format all..
(use-package format-all
  :hook (prog-mode . format-all-mode)
  :bind (("C-c f" . format-all-buffer)))

;; setup the font
(set-face-attribute 'default nil
                    :family "Lilex Nerd Font Mono"
                    :height 160)

(set-frame-font "Lilex Nerd Font Mono-16" nil t)
(use-package ligature
  :ensure t
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

;;; editor.el ends here
