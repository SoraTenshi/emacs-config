;;; city.el --- Get some information about arbitrary cities -*- lexical-binding: t; -*-
;;; Commentary:
;; City stuff
;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(cl-defstruct geocoding-result
  name
  latitude
  longitude
  country
  timezone
  population
  admin1
  admin2
  admin3
  admin4)

(cl-defstruct geocoding-response
  results generationtime_ms)

(defun parse-geocoding-result (alist)
  "Create a `geocoding-result` from ALIST."
  (make-geocoding-result
   :name        (alist-get 'name        alist)
   :latitude    (alist-get 'latitude    alist)
   :longitude   (alist-get 'longitude   alist)
   :country     (alist-get 'country     alist)
   :timezone    (alist-get 'timezone    alist)
   :population  (alist-get 'population  alist)
   :admin1      (alist-get 'admin1      alist)
   :admin2      (alist-get 'admin2      alist)
   :admin3      (alist-get 'admin3      alist)
   :admin4      (alist-get 'admin4      alist)))

(defun parse-geocoding-response (json-string)
  "Parse the JSON-STRING response into `geocoding-result`."
  (let* ((data (json-parse-string json-string :object-type 'alist))
         (results (alist-get 'results data)))
    (make-geocoding-response
     :generationtime_ms (alist-get 'generationtime_ms data)
     :results           (mapcar #'parse-geocoding-result (append results nil)))))

(defun city--city-information (city callback)
  "Fetch geocoding info for CITY, call CALLBACK with a `geocoding-result'."
  (url-retrieve
   (format "https://geocoding-api.open-meteo.com/v1/search?name=%s&count=1"
           (url-hexify-string city))
   (lambda (_status)
     (goto-char (point-min))
     (re-search-forward "^$")
     (forward-char)
     (let* ((body     (buffer-substring-no-properties (point) (point-max)))
            (response (parse-geocoding-response body))
            (results  (geocoding-response-results response)))
       (if results
           (funcall callback (car results))
         (message "No results found for city: %s" city))))))

(defun city-local-time (city)
  "Fetch the local time of CITY."
  (interactive "sCity: ")
  (city--city-information
   city
   (lambda (result)
     (let* ((tz       (geocoding-result-timezone result))
            (name     (geocoding-result-name result))
            (country  (geocoding-result-country result))
            (time-str (format-time-string "%Y-%m-%d %H:%M:%S" (current-time) tz)))
       (message "%s, %s [%s]: %s" name country tz time-str)))))

(provide 'city)
;;; city.el ends here
