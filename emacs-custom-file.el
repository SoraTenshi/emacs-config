;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window aggressive-indent cape cl-format colorful-mode
                consult-eglot consult-flycheck corfu devdocs diminish
                docker dockerfile-mode embark-consult erc-image
                fancy-compilation flycheck-eglot format-all gcmh
                go-mode hel helpful highlight-indent-guides htmlize
                kind-icon magit marginalia nix-mode odin-mode
                orderless org-modern org-roam paredit pcre2el plz
                rust-mode simpc-mode vdiff vertico vundo zig-mode))
 '(safe-local-variable-values
   '((eval setq-local compile-command
           (format "%s --batch -l build.el -f build/all"
                   (shell-quote-argument
                    (expand-file-name invocation-name
                                      invocation-directory)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
