;;; keybinds-helix.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(hel-define-command delete-char-under (count)
  "Deletes the character under the cursor."
  :multiple-cursors t
  :merge-selections t
  (interactive "*p")
  (unless (region-active-p)
    (forward-char 1))
  (hel-cut count))

(hel-define-command change-char-under ()
  "Change char under cursor and enters insert mode."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
   (unless (region-active-p)
     (forward-char 1)))
  (hel-change))

(hel-keymap-global-set :state 'normal
  "g h" 'hel-beginning-of-line-command
  "g s" 'hel-first-non-blank
  "g e" 'hel-end-of-buffer
  "d"   'delete-char-under
  "c"   'change-char-under
  "G"   nil)

;;; keybinds-helix.el ends here
