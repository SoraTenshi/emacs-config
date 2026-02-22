;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("011fa19868867fb5e2e12f0b2b99a43ad81edaa7f1b16d5e6aae016272b1d7a9"
     "289c474ad4d50a9924625a374e0ac0315ea61d6dcbe5af3876f09a90c1575338"
     default))
 '(package-selected-packages
   '(ace-window aggressive-indent breadcrumb cape cl-format colorful-mode
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
