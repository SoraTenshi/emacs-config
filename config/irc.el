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
        (format "[%s:%s] "
                (erc-network-name)
                (or (erc-default-target) ""))))

(add-hook 'erc-mode-hook
          (lambda ()
            (setq-local buffer-display-table nil)
            (whitespace-mode -1)))

;;; irc.el ends here
