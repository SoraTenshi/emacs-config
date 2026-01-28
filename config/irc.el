;;; irc.el --- IRC settings -*- lexical-bindings: t; -*-
;;; Commentary:
;; IRC configuration
;;; Code:

(defun connect-computer ()
  "Connect to the computer via IRC."
  (interactive)
  (erc-tls :server "colonq.computer" :port 26697 :nick "soranotenshi")
  (select-window (get-buffer (current-buffer))))

(setq erc-prompt
      (lambda ()
        (format "[%s] [%s] <%s> "
                (format-time-string "%H:%M")
                (buffer-name)
                (erc-current-nick)))
      erc-insert-timestamp-function #'erc-insert-timestamp-left)

(add-hook 'erc-mode-hook
          (lambda ()
            (setq-local buffer-display-table nil)
            (whitespace-mode -1)
            (show-paren-mode -1)))

;;; irc.el ends here
