;;; weather-functions.el --- Weather display for EXWM mode-line -*- lexical-binding: t; -*-
;;; Commentary:
;; Displays current weather in the mode-line using Open-Meteo API.
;; Automatically detects location via IP geolocation or can query by city name.
;; The weather updates automatically every 10 minutes in the mode-line.
;;; Code:

(require 'url)
(require 'json)

(defvar exwm-weather-string "…"
  "Current weather string displayed in mode-line.")
(defvar exwm-weather--geo nil
  "Cached geolocation coordinates as (latitude longitude city country).")
(defvar exwm-weather-update-interval 600
  "Weather update interval in seconds.")

(defconst exwm-weather--code-map
  '((0 . "Clear sky")
    (1 . "Mainly clear")
    (2 . "Partly cloudy")
    (3 . "Overcast")
    (45 . "Fog")
    (48 . "Rime fog")
    (51 . "Light drizzle")
    (53 . "Moderate drizzle")
    (55 . "Dense drizzle")
    (56 . "Light freezing drizzle")
    (57 . "Dense freezing drizzle")
    (61 . "Slight rain")
    (63 . "Moderate rain")
    (65 . "Heavy rain")
    (66 . "Light freezing rain")
    (67 . "Heavy freezing rain")
    (71 . "Slight snow")
    (73 . "Moderate snow")
    (75 . "Heavy snow")
    (77 . "Snow grains")
    (80 . "Slight rain showers")
    (81 . "Moderate rain showers")
    (82 . "Violent rain showers")
    (85 . "Slight snow showers")
    (86 . "Heavy snow showers")
    (95 . "Thunderstorm")
    (96 . "Thunderstorm with hail")
    (99 . "Thunderstorm with heavy hail"))
  "Mapping of WMO weather codes to descriptive strings.")

(defun exwm-weather--state (code)
  "Get full weather description for CODE."
  (or (alist-get code exwm-weather--code-map) "Unknown"))

(defun exwm-weather--geocode-city (city callback)
  "Get coordinates for CITY and call CALLBACK with lat, lon, name, country."
  (url-retrieve
   (format "https://geocoding-api.open-meteo.com/v1/search?name=%s&count=1"
           (url-hexify-string city))
   (lambda (_status)
     (goto-char (point-min))
     (re-search-forward "^$")
     (forward-char)
     (let* ((json (json-parse-buffer :object-type 'alist))
            (results (alist-get 'results json)))
       (if (and results (> (length results) 0))
           (let ((res (aref results 0)))
             (funcall callback
                      (alist-get 'latitude res)
                      (alist-get 'longitude res)
                      (alist-get 'name res)
                      (alist-get 'country res)))
         (message "No results found for city: %s" city))))))

(defun exwm-weather--fetch (lat lon callback)
  "Fetch weather at LAT LON, call CALLBACK with current data and timezone."
  (url-retrieve
   (format (concat "https://api.open-meteo.com/v1/forecast?"
                   "latitude=%s&longitude=%s"
                   "&current=temperature_2m,apparent_temperature,"
                   "weather_code,wind_speed_10m,relative_humidity_2m"
                   "&timezone=auto")
           lat lon)
   (lambda (_status)
     (goto-char (point-min))
     (re-search-forward "^$")
     (forward-char)
     (let ((json (json-parse-buffer :object-type 'alist)))
       (funcall callback
                (alist-get 'current json)
                ;; t is to make the url-retrieve silent.
                (alist-get 'timezone json)))) nil t))

(defun exwm-weather--ensure-geo (callback)
  "Ensure geo coordinates are cached, then call CALLBACK."
  (if exwm-weather--geo
      (funcall callback)
    (url-retrieve
     "http://ip-api.com/json/"
     (lambda (_status)
       (goto-char (point-min))
       (re-search-forward "^$")
       (forward-char)
       (let ((json (json-parse-buffer :object-type 'alist)))
         (setq exwm-weather--geo
               (list (alist-get 'lat json)
                     (alist-get 'lon json)
                     (alist-get 'city json)
                     (alist-get 'country json)))
         (funcall callback)))
     ;; t is for silent.
     nil t)))

(defun exwm-weather-update ()
  "Update weather string for mode-line using IP-based geolocation."
  (interactive)
  (exwm-weather--ensure-geo
   (lambda ()
     (pcase-let ((`(,lat ,lon ,city ,country) exwm-weather--geo))
       (exwm-weather--fetch
        lat lon
        (lambda (cur _timezone)
          (let* ((temp (alist-get 'temperature_2m cur))
                 (feels (alist-get 'apparent_temperature cur))
                 (wind (alist-get 'wind_speed_10m cur))
                 (hum (alist-get 'relative_humidity_2m cur))
                 (code (alist-get 'weather_code cur))
                 (tooltip (format "City: %s, %s\nTemperature: %.1f°C\nFeels like: %.1f°C\nCondition: %s\nHumidity: %d%%\nWind: %.1f m/s"
                                  city country
                                  temp feels (exwm-weather--state code) hum wind)))
            (setq exwm-weather-string
                  (propertize (format "(%d°C)" (round temp))
                              'help-echo tooltip))
            (force-mode-line-update t))))))))

(defun exwm-weather-city (city &optional debug)
  "Fetch and display detailed weather for CITY.
If DEBUG is non-nil, show raw JSON response."
  (interactive "sCity: \nP")
  (exwm-weather--geocode-city
   city
   (lambda (lat lon name country)
     (exwm-weather--fetch
      lat lon
      (lambda (cur timezone)
        (let* ((temp (alist-get 'temperature_2m cur))
               (feels (alist-get 'apparent_temperature cur))
               (wind (alist-get 'wind_speed_10m cur))
               (hum (alist-get 'relative_humidity_2m cur))
               (code (alist-get 'weather_code cur))
               (time-str (alist-get 'time cur))
               (time-obj (ignore-errors (date-to-time time-str)))
               (time-formatted (if time-obj
                                   (format-time-string "%Y-%m-%d %H:%M" time-obj)
                                 time-str)))
          (with-current-buffer (get-buffer-create (if debug "*Weather Debug*" "*Weather*"))
            (erase-buffer)
            (if debug
                (insert (format "City: %s\nLat: %s, Lon: %s, Country: %s\n\n%s"
                                city lat lon country
                                (pp-to-string (list :current cur :timezone timezone))))
              (progn
                (insert (format "%s, %s\n\n" name country))
                (insert (format "Time:        %s (%s)\n" time-formatted timezone))
                (insert (format "Temperature: %.1f °C\n" temp))
                (insert (format "Feels like:  %.1f °C\n" feels))
                (insert (format "Condition:   %s\n" (exwm-weather--state code)))
                (insert (format "Humidity:    %d %%\n" hum))
                (insert (format "Wind:        %.1f m/s\n" wind))))
            (goto-char (point-min))
            (display-buffer (current-buffer)))
          (message "%d°C (%d) %s (%s)"
                   (round temp)
                   (round feels)
                   (exwm-weather--state code)
                   name)))))))

(defun exwm-weather-city-debug (city)
  "Fetch and display detailed weather for CITY in debug mode."
  (interactive "sCity: ")
  (exwm-weather-city city t))

(defvar exwm-weather--mode-line
  '(:eval exwm-weather-string)
  "Mode-line construct for weather display.")

(unless (member exwm-weather--mode-line global-mode-string)
  (setq global-mode-string
        (append global-mode-string (list exwm-weather--mode-line))))

(run-with-timer 0 exwm-weather-update-interval #'exwm-weather-update)

(provide 'weather)
;;; weather.el ends here
