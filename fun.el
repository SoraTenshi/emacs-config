;; fun.el --- Just some fun stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Fun features brought to EMACS.
;;; Code:

(require 'url)

(defun fun--disable-scroll (buf win)
  "Disable all scrolling for BUF/WIN."
  (with-current-buffer buf
    (setq-local scroll-bar-mode nil
                vertical-scroll-bar nil)
    (set-window-scroll-bars win 0 nil 0 nil)
    (dolist (key '([wheel-up] [wheel-down]
                   [double-wheel-up] [double-wheel-down]
                   [triple-wheel-up] [triple-wheel-down]))
      (local-set-key key #'ignore))))

(defun fun--skip-header ()
  "Skip HTTP header."
  (goto-char (point-min))
  (re-search-forward "\n\n"))

(defun fun/display-cat ()
  "Cat... we need more CATS!!!!"
  (interactive)
  (url-retrieve
   "https://cataas.com/cat"
   (lambda (_)
     (fun--skip-header)
     (let* ((data (buffer-substring-no-properties (point) (point-max)))
            (buf  (get-buffer-create "*cat*"))
            (win  (display-buffer buf))
            (w    (window-pixel-width win))
            (h    (window-pixel-height win))
            (img  (create-image (encode-coding-string data 'binary)
                                nil t
                                :max-width w :max-height h)))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert-image img)
           (goto-char (point-min)))
         (fun--disable-scroll buf win))))))

(defun fun/display-fox ()
  "Fox... we need more FLOOOF!!!!"
  (interactive)
  (fun--fetch-image "https://some-random-api.com/animal/fox"
                    (lambda (json) (gethash "image" json))
                    "fox"))

(defun fun--fetch-image (api json-key buffer)
  "Fetch image from API using JSON-KEY fn to extract image to BUFFER."
  (url-retrieve
   api
   (lambda (_)
     (fun--skip-header)
     (let* ((json    (json-parse-buffer))
            (img-url (funcall json-key json)))
       (url-retrieve
        img-url
        (lambda (_)
          (fun--skip-header)
          (let* ((data (buffer-substring-no-properties (point) (point-max)))
                 (buf  (get-buffer-create (format "*%s*" buffer)))
                 (win  (display-buffer buf))
                 (w    (window-pixel-width win))
                 (h    (window-pixel-height win))
                 (img  (create-image (encode-coding-string data 'binary)
                                     nil t
                                     :max-width w :max-height h)))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert-image img)
                (goto-char (point-min)))
              (fun--disable-scroll buf win)))))))))

(defun fun--fetch-anime-image (type)
  "Fetch image from nekos.best api and extract image TYPE to buffer."
  (fun--fetch-image (format "https://nekos.best/api/v2/%s" type)
                    (lambda (json)
                      (gethash "url" (aref (gethash "results" json) 0)))
                    type))

(defun fun/display-kitsune ()
  "Because... Foxgirls have fluffy tail!"
  (interactive)
  (fun--fetch-anime-image "kitsune"))

(defun fun/display-waifu ()
  "Classical Waifu image.  No much to say there."
  (interactive)
  (fun--fetch-anime-image "waifu"))

(defun fun/display-neko ()
  "What's better than girls? Girls with tails and cat ears!"
  (interactive)
  (fun--fetch-anime-image "neko"))

(defun fun/fox-fact ()
  "Print a fox fact into the message buffer."
  (interactive)
  (url-retrieve "https://api.some-random-api.com/facts/fox"
                (lambda (_)
                  (fun--skip-header)
                  (let* ((json (json-parse-buffer))
                         (fact (gethash "fact" json)))
                    (message "%s" fact)))))

(defun fun/display-ragdoll ()
  "Ragdolls are the best cats, change my mind."
  (interactive)
  (fun--fetch-image "https://api.thecatapi.com/v1/images/search?breed_ids=ragd"
                    (lambda (json) (gethash "url" (aref json 0)))
                    "ragdoll"))

(provide 'fun)
;;; fun.el ends here
