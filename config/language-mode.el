;;; language-mode.el --- Language specific settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Just loads all the language files
;;; Code:
(load-config "config/language/c.el")
(load-config "config/language/common-lisp.el")
(load-config "config/language/elixir.el")
(load-config "config/language/go.el")
;;
;;(load-config "config/language/jai.el")
(load-config "config/language/rust.el")
(load-config "config/language/scheme.el")
(load-config "config/language/zig.el")
(load-config "config/language/nix.el")

(defmacro make-compile-command (lang command)
  "Create hook for LANG to create the compile command COMMAND."
  `(add-hook ',(intern (concat (symbol-name lang) "-mode-hook"))
             (lambda ()
               (set (make-local-variable 'compile-command) ,command))))

(make-compile-command rust "cargo build")
(make-compile-command go "go build")
(make-compile-command zig "zig build")

;;; language-mode.el ends here
