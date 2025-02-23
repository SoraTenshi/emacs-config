;;; editor.el ---
;; Basic configuration / Setup for emacs
;;; Code:

;; enable line (relative) numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; remove the "tool" bar on top
(tool-bar-mode -1)
(menu-bar-mode -1)

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
                    :height 140)

;; boo! it doesn't work yet.
(use-package ligature
  :config
  (global-ligature-mode t)
  (ligature-set-ligatures 't '("www")))
;;("++" "+++" "&&" "%%" "{|" "|}" "[|" "|]" "|>" "<|" "<>" "::=" "--" "~~" "#[" "]#" "!=" ".." "..." ".<" ".?" ".=" "..=" "::" ":::" ":=" ";;" "!!" "!==" "/=" "<*>" "/**" "/*" "#(" ")#" "#!" "/>" "</" "</>" "<<" "<<<" "~~" ">=" "<=" "//" "///" "**" "^=" "?=" "#=")))



;;; editor.el ends here
