;;; language-mode.el --- Language specific settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Just loads all the language files
;;; Code:
(load-config "config/language/c.el")
(load-config "config/language/common-lisp.el")
;;(load-config "config/language/elixir.el")
(load-config "config/language/go.el")
;;
;;(load-config "config/language/jai.el")
(load-config "config/language/rust.el")
;;(load-config "config/language/scheme.el")
(load-config "config/language/zig.el")
(load-config "config/language/nix.el")
(load-config "config/language/yuck.el")

(defun lang/display-modes ()
  "Enable `display-line-numbers-mode` and `whitespace-mode`."
  (display-line-numbers-mode 1)
  (font-lock-mode 1)
  (whitespace-mode 1))

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

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)

;;; language-mode.el ends here
