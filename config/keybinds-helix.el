;;; keybinds-helix.el --- Description here -*- lexical-binding: t; -*-
;;; Commentary:
;; Blabla.
;;; Code:

(require 'helix)

(defun helix-forward-word-no-space ()
  "Move to next word."
  (interactive)
  (helix--with-movement-surround
   (when (re-search-forward "\\([[:alnum:]_]+\\)\\|\\([[:punct:]]+\\)\\|\n" nil 'move)
     (or (eq (char-before (match-end 0)) ?\n)
         (if (match-string 1)
             (skip-syntax-forward "w")
           (skip-syntax-forward ".()"))))))

(defun helix-select-line-up ()
  "Select the current line, extending upward on every subsequent call."
  (interactive)
  (if (and (region-active-p) (bolp))
      (progn
        (call-interactively #'previous-line)
        (beginning-of-line))
    (end-of-line)
    (set-mark-command nil)
    (beginning-of-line)))

(defun helix-change-mode ()
  "Delete selection and enter insert mode."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char 1))
  (helix--clear-data)
  (helix-insert))

(defun helix-scroll-down ()
  "Scroll down half a page."
  (interactive)
  (let ((half-page (/ (window-height) 2)))
    (forward-line half-page)
    (recenter)))

(defun helix-scroll-up ()
  "Scroll up half a page."
  (interactive)
  (let ((half-page (/ (window-height) 2)))
    (forward-line (- half-page))
    (recenter)))

(with-eval-after-load 'helix
  (advice-add #'nav/global-search :before #'better-jumper-set-jump)
  (advice-add #'xref-find-references :before #'better-jumper-set-jump)
  (advice-add #'project/switch :before #'better-jumper-set-jump)
  (advice-add #'consult-project-buffer :before #'better-jumper-set-jump)
  (advice-add #'nav/next-diagnostic :before #'better-jumper-set-jump)
  (advice-add #'nav/prev-diagnostic :before #'better-jumper-set-jump)
  (advice-add #'centaur-tabs-forward :before #'better-jumper-set-jump)
  (advice-add #'centaur-tabs-backward :before #'better-jumper-set-jump)
  (advice-add #'centaur-tabs-buffer-list :before #'better-jumper-set-jump)
  (advice-add #'buffer/centaur-switch-to-group :before #'better-jumper-set-jump)

  (helix-define-key 'normal (kbd "]d") #'nav/next-diagnostic)
  (helix-define-key 'normal (kbd "[d") #'nav/prev-diagnostic)
  (helix-define-key 'normal "X" #'helix-select-line-up)
  (helix-define-key 'normal "e" #'helix-forward-word-no-space)
  (helix-define-key 'normal "c" #'helix-change-mode)
  (helix-define-key 'normal "U" #'undo-redo)
  (helix-define-key 'normal (kbd "C-d") #'helix-scroll-down)
  (helix-define-key 'normal (kbd "C-u") #'helix-scroll-up)

  (helix-define-key 'space "k" #'ui/show-popup-doc)
  (helix-define-key 'space "d" #'ui/diagnostic-list)
  (helix-define-key 'space "r" #'eglot-rename)
  (helix-define-key 'space "a" #'eglot-code-action)
  (helix-define-key 'space "f" #'find-file)
  (helix-define-key 'space "b" #'consult-project-buffer)
  (helix-define-key 'space "/" #'nav/global-search)
  (helix-define-key 'space "w" #'kill-buffer)
  (helix-define-key 'space "p" #'project/switch)
  (helix-define-key 'space "B" #'buffer/centaur-switch-to-group)
  (helix-define-key 'space "R" #'clipboard/replace-region)

  (helix-define-key 'goto "n" #'centaur-tabs-forward)
  (helix-define-key 'goto "p" #'centaur-tabs-backward)
  (helix-define-key 'goto "b" #'centaur-tabs-buffer-list)

  (helix-define-key 'window "q" #'quit/safe-exit)

  ;; TODO: implement those
  ;; (evil-define-key 'normal 'global (kbd "SPC w") #'evil-delete-buffer)
  ;; (evil-define-key '(normal visual) 'global (kbd "C-o") #'evil-jump-backward)
  ;; (evil-define-key '(normal visual) 'global (kbd "C-i") #'evil-jump-forward)

  (helix-define-key 'normal (kbd "m") (make-sparse-keymap))
  (helix-define-key 'normal (kbd "m d") #'manipulation/unsurround)
  (helix-define-key 'normal (kbd "m s") #'manipulation/surround)
  (helix-define-key 'normal (kbd "m m") #'manipulation/jump-item)
  (helix-define-key 'normal (kbd "m i") #'manipulation/select-inside)
  (helix-define-key 'normal (kbd "=") #'format-all-buffer)
  (helix-define-key 'normal (kbd "SPC s") #'comment/toggle)
  (helix-define-key 'normal (kbd "SPC c") #'compile)
  (helix-define-key 'normal (kbd "%") #'select/whole-buffer)

  )

;;; keybinds-helix.el ends here
