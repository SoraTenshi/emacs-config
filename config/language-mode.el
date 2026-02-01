;;; language-mode.el --- Language specific settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Just loads all the language files
;;; Code:
(load-config "config/language/c.el")
(load-config "config/language/common-lisp.el")
;;(load-config "config/language/elixir.el")
(load-config "config/language/go.el")
(load-config "config/language/odin.el")
;;(load-config "config/language/jai.el")
(load-config "config/language/rust.el")
;;(load-config "config/language/scheme.el")
(load-config "config/language/zig.el")
(load-config "config/language/nix.el")
(load-config "config/language/yuck.el")
(load-config "config/language/docker.el")

(defun lang/display-modes ()
  "Enable `display-line-numbers-mode` and `whitespace-mode`."
  (whitespace-mode 1)
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(add-hook 'c-mode-hook 'lang/display-modes)
(add-hook 'scheme-mode-hook 'lang/display-modes)
(add-hook 'rust-mode-hook 'lang/display-modes)
(add-hook 'zig-mode-hook 'lang/display-modes)
(add-hook 'yuck-mode-hook 'lang/display-modes)

(defmacro make-compile-command (lang command)
  "Create hook for LANG to create the compile command COMMAND."
  `(add-hook ',(intern (concat (symbol-name lang) "-mode-hook"))
             (lambda ()
               (set (make-local-variable 'compile-command) ,command))))

(make-compile-command rust "nix develop -c cargo check")
(make-compile-command go "go build")
(make-compile-command zig "zig build")

(global-font-lock-mode t)
(add-hook 'prog-mode-hook 'font-lock-mode)

(use-package fancy-compilation
  :straight t
  :custom (fancy-compilation-override-colors)
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package ob-zig
  :straight (:type git :host github :repo "jolby/ob-zig.el")
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(zig . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(use-package ob-rust
  :straight t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(rust . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(setq org-babel-lisp-eval-fn 'sly-eval)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (rust . t)
   (zig . t)))
(org-babel-do-load-languages 'org-babel-load-languages
                             org-babel-load-languages)

(eval-after-load 'org
  '(progn
     (require 'org-tempo)
     (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
     (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
     (add-to-list 'org-structure-template-alist '("rust" . "src rust"))
     (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
     (add-to-list 'org-structure-template-alist '("zig" . "src zig"))))

;;; language-mode.el ends here
