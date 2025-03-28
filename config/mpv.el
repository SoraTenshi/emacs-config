;;; mpv.el --- mpv stuff -*- lexical-binding: t; -*-
;;; Commentary:
;; mpv control
;;; Code:

(use-package mpv
  :ensure t
  ;; 1.1 volume-step seems to be the most sane
  :config (setq mpv-volume-step 1.1))

(defconst *agqr-url* "https://www.uniqueradio.jp/agapps/hls/cdn.m3u8")

(defun play-agqr ()
  "Play agqr hls stream via mpv."
  (interactive)
  (mpv-start "--no-video" "--force-window=no" *agqr-url*))

;;; mpv.el ends here
