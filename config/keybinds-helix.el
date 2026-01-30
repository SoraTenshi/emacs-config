;;; keybinds-helix.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(hel-keymap-global-set :state 'normal
  "g h" 'hel-beginning-of-line-command
  "g s" 'hel-first-non-blank
  "g e" 'hel-end-of-buffer
  "G"   nil)

;;; keybinds-helix.el ends here
