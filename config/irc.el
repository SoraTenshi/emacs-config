;;; irc.el --- IRC settings -*- lexical-bindings: t; -*-
;;; Commentary:
;; IRC configuration
;;; Code:

(defun connect-computer ()
  "Connect to the computer via IRC."
  (interactive)
  (let ((lcolonq (erc-tls :server "colonq.computer"
                          :port 26697
                          :nick "soranotenshi")))
    (run-with-timer 2 nil
                    (lambda ()
                      (when (buffer-live-p lcolonq)
                        (with-current-buffer lcolonq
                          (erc-join-channel "#cyberspace")))))
    (let ((check-timer nil))
      (setq check-timer
            (run-with-timer 1 5
                            (lambda ()
                              (let ((buf (get-buffer "#cyberspace@LCOLONQ")))
                                (when buf
                                  (switch-to-buffer buf)
                                  (cancel-timer check-timer)))))))))

(setq erc-prompt
      (lambda ()
        (format "[%s] [%s] <%s>"
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
