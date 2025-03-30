;;; spotify.el --- Possibly a spotify player for EMACS? -*- lexical-binding: t; -*-
;;; Commentary:
;; Because i am too lazy to switch clients lmao.
;;; Code:

(defun load-secrets ()
  "Load secret file."
  (with-eval-after-load 'smudge
    (let ((secrets (expand-file-name "secrets/spotify.el" configuration-root)))
      (if (file-exists-p secrets)
          (load-file secrets)
        (message "WARNING: Smudge cannot be started to to missing secrets '%s'." secrets)))))

(use-package smudge
  :ensure t
  :init
  (load-secrets)
  :custom
  (smudge-oauth2-client-id "a31391ec737a4b17ace31f7cb0cab15a")
  (smudge-oauth2-client-secret "6a34aabee0fa42c1a0e20d181ca45704")
  (smudge-transport 'connect)
  (smudge-status-location 'modeline)
  :config
  (global-smudge-remote-mode))

(evil-define-key 'normal 'global (kbd "SPC s") 'smudge-command-map)

;;; spotify.el ends here
