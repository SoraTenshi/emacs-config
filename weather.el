;;; weather-functions.el --- Weather display for EXWM mode-line -*- lexical-binding: t; -*-
;;; Commentary:
;; Displays current weather in the mode-line using Open-Meteo API.
;; Automatically detects location via IP geolocation or can query by city name.;; The weather updates automatically every 10 minutes in the mode-line.
;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

;; ========================================================================
;; Options
;; ========================================================================

(defgroup weather nil
  "Modeline weather based on this IP."
  :group 'mode-line
  :prefix "weather-")

(defcustom weather-update-interval 600
  "Seconds between re-fetching."
  :type 'integer)

(defcustom weather-temperature-unit 'celsius
  "Unit for temperature display."
  :type '(choice (const :tag "Celsius"    celsius)
                 (const :tag "Fahrenheit" fahrenheit)
                 (const :tag "Kelvin"     kelvin)))

(defcustom weather-geo-ttl 300
  "Seconds before the TTL (Time to Live) is stale.
0 is always refetch."
  :type 'integer)

(defcustom weather-mode-line-format "(%s°%s)"
  "Format string for the modeline weather string."
  :type 'string)

;; ========================================================================
;; Internal State
;; ========================================================================


(defvar weather--string ""
  "Current modeline weather string.")

(defvar weather--geo nil
  "Cached geolocation.")

(defvar weather--geo-time nil
  "Timestamp as to when the geolocation was fetched.")

(defvar weather--timer nil
  "Timer object for periodic weather updates, managed by `weather-mode`.")

(defconst weather--wmo-codes
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

(defun weather--wmo-description (code)
  "Description for the WMO weather CODE.  Otherwise `UNKNOWN`."
  (or (alist-get code weather--wmo-codes) "Unknown"))

;; ========================================================================
;; Helpers
;; ========================================================================

(defconst weather--unit-letters
  '((celsius    . "C")
    (fahrenheit . "F")
    (kelvin     . "K"))
  "Alist mapping unit symbols to display letters.")

(defun weather--unit-letter ()
  "Return the shorthand for the temperature unit."
  (or (cdr (assq weather-temperature-unit weather--unit-letters))
      (error "Unknown unit %S" weather-temperature-unit)))

(defun weather--to-display-temp (celsius)
  "Convert CELSIUS to the configured unit."
  (pcase weather-temperature-unit
    ('fahrenheit (+ (* celsius 1.8) 32.0))
    ('kelvin     (+ celsius 273.15))
    ('celsius    celsius)
    (unit        (error "Unknown unit %S" unit))))

;; ========================================================================
;; Network utilities
;; ========================================================================

(defun weather--parse-json-response ()
  "Parse the JSON HTTP response."
  ;; skip http headers
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (json-parse-buffer :object-type 'alist))

(defun weather--retrieve (url callback)
  "Fetch URL async, parse JSON and call CALLBACK with the result."
  (url-retrieve url
                (lambda (_status)
                  (funcall callback (weather--parse-json-response)))
                nil
                t
                t))

;; ========================================================================
;; Geolocation
;; ========================================================================

(defun weather--geo-stale-p ()
  "Returns non-nil if the geo cache is stale."
  (or (null weather--geo)
      (null weather--geo-time)
      (> (- (float-time) weather--geo-time) weather-geo-ttl)))

(defun weather--ensure-geo (callback)
  "Call CALLBACK after ensuring `weather--geo' is fresh."
  (if (not (weather--geo-stale-p))
      (funcall callback)
    (weather--retrieve
     "http://ip-api.com/json/"
     (lambda (json)
       (setq weather--geo
             (list (alist-get 'lat     json)
                   (alist-get 'lon     json)
                   (alist-get 'city    json)
                   (alist-get 'country json))
             weather--geo-time (float-time))
       (funcall callback)))))

(defun weather--geocode-city (city callback)
  "Geocode CITY and call CALLBACK."
  (weather--retrieve
   (format "https://geocoding-api.open-meteo.com/v1/search?name=%s&count=1"
           (url-hexify-string city))
   (lambda (json)
     (let ((results (alist-get 'results json)))
       (if (and results (> (length results) 0))
           (let ((hit (aref results 0)))
             (funcall callback
                      (alist-get 'latitude  hit)
                      (alist-get 'longitude hit)
                      (alist-get 'name      hit)
                      (alist-get 'country   hit)))
         (message "no geocoding results for %S" city))))))

;; ========================================================================
;; Weather
;; ========================================================================

(defconst weather--api-url
  (concat "https://api.open-meteo.com/v1/forecast"
          "?latitude=%s&longitude=%s"
          "&current=temperature_2m,apparent_temperature"
          ",weather_code,wind_speed_10m,relative_humidity_2m"
          "&timezone=auto")
  "Open-Meteo API URL template.")

(defun weather--fetch (lat lon callback)
  "Fetch current weather at LAT/LON and call CALLBACK with."
  (weather--retrieve
   (format weather--api-url lat lon)
   (lambda (json)
     (funcall callback
              (alist-get 'current  json)
              (alist-get 'timezone json)))))

;; ========================================================================
;; Modeline updates
;; ========================================================================

(defun weather--build-mode-line-string (current city country)
  "Return a mode-line string from CURRENT for CITY in COUNTRY."
  (let* ((raw-temp (alist-get 'temperature_2m      current))
         (raw-feel (alist-get 'apparent_temperature current))
         (wind     (alist-get 'wind_speed_10m       current))
         (hum      (alist-get 'relative_humidity_2m current))
         (code     (alist-get 'weather_code         current))
         (unit     (weather--unit-letter))
         (temp     (weather--to-display-temp raw-temp))
         (feel     (weather--to-display-temp raw-feel))
         (tooltip  (format "City: %s, %s\nTemperature: %.1f°%s\nFeels like: %.1f°%s\nCondition: %s\nHumidity: %d%%\nWind: %.1f m/s"
                           city country
                           temp unit feel unit
                           (weather--wmo-description code)
                           hum wind)))
    (propertize (format weather-mode-line-format (round temp) unit)
                'help-echo tooltip
                'mouse-face 'mode-line-highlight
                'local-map  (let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1]
                                          #'weather-update)
                              map))))

;;;###autoload
(defun weather-update ()
  "Refresh the mode-line weather string using IP-based geolocation."
  (interactive)
  (weather--ensure-geo
   (lambda ()
     (pcase-let ((`(,lat ,lon ,city ,country) weather--geo))
       (weather--fetch lat lon
                       (lambda (current _tz)
                         (setq weather--string
                               (weather--build-mode-line-string current city country))
                         (force-mode-line-update t)))))))

;; ========================================================================
;; Interactive
;; ========================================================================

(defun weather--format-detail-buffer (buf name country current timezone &optional debug-data)
  "Fill BUF with formatted CURRENT weather details for NAME/COUNTRY or raw DEBUG-DATA."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if debug-data
          (insert debug-data)
        (let* ((raw-temp (alist-get 'temperature_2m      current))
               (raw-feel (alist-get 'apparent_temperature current))
               (wind     (alist-get 'wind_speed_10m       current))
               (hum      (alist-get 'relative_humidity_2m current))
               (code     (alist-get 'weather_code         current))
               (time-str (alist-get 'time                 current))
               (time-fmt (condition-case nil
                             (format-time-string "%Y-%m-%d %H:%M"
                                                 (date-to-time time-str))
                           (error time-str)))
               (unit     (weather--unit-letter))
               (temp     (weather--to-display-temp raw-temp))
               (feel     (weather--to-display-temp raw-feel)))
          (insert (format "%-13s %s, %s\n"   "Location:"   name country))
          (insert (format "%-13s %s (%s)\n"  "Time:"       time-fmt timezone))
          (insert (format "%-13s %.1f °%s\n" "Temperature:" temp unit))
          (insert (format "%-13s %.1f °%s\n" "Feels like:"  feel unit))
          (insert (format "%-13s %s\n"       "Condition:"  (weather--wmo-description code)))
          (insert (format "%-13s %d %%\n"    "Humidity:"   hum))
          (insert (format "%-13s %.1f m/s\n" "Wind:"       wind))))
      (goto-char (point-min))
      (read-only-mode)))
  (display-buffer buf))

;;;###autoload
(defun weather/city (city &optional debug)
  "Fetch and display weather for CITY in a dedicated buffer.
With prefix argument DEBUG (or when called from `weather-city-debug'),
shows raw JSON data instead."
  (interactive "sCity: \nP")
  (weather--geocode-city
   city
   (lambda (lat lon name country)
     (weather--fetch
      lat lon
      (lambda (current timezone)
        (let ((buf (get-buffer-create (if debug "*Weather Debug*" "*Weather*"))))
          (weather--format-detail-buffer
           buf name country current timezone
           (when debug
             (format "City: %s\nLat/Lon: %s, %s\nCountry: %s\n\n%s"
                     city lat lon country
                     (pp-to-string (list :current current :timezone timezone)))))
          (message "%d°%s  %s  (%s)"
                   (round (weather--to-display-temp (alist-get 'temperature_2m current)))
                   (weather--unit-letter)
                   (weather--wmo-description (alist-get 'weather_code current))
                   name)))))))

;;;###autoload
(defun weather/city-debug (city)
  "Like `weather-city` for CITY but always show the raw JSON response."
  (interactive "sCity: ")
  (weather-city city t))

;;;###autoload
(defun weather/clear-geo-cache ()
  "Clear the cached geolocation so the next update re-detects it."
  (interactive)
  (setq weather--geo nil
        weather--geo-time nil)
  (message "geolocation cache for `weather-mode` cleared"))

;; ========================================================================
;; Minor mode
;; ========================================================================

(defvar weather--mode-line-entry
  '(:eval (when (bound-and-true-p weather-mode) weather--string))
  "Mode-line construct inserted by `weather-mode'.")

;;;###autoload
(define-minor-mode weather-mode
  "Toggle weather display in the mode-line.

When enabled, the current temperature appears in `global-mode-string'
and is refreshed every `weather-update-interval' seconds.
Clicking the temperature in the mode-line triggers a manual refresh."
  :global t
  :group 'weather
  (if weather-mode
      (progn
        (unless (member weather--mode-line-entry global-mode-string)
          (setq global-mode-string
                (append global-mode-string (list weather--mode-line-entry))))
        (setq weather--timer
              (run-with-timer 0 weather-update-interval #'weather-update)))
    (when weather--timer
      (cancel-timer weather--timer)
      (setq weather--timer nil))
    (setq global-mode-string
          (delete weather--mode-line-entry global-mode-string))
    (setq weather--string "")))

(provide 'weather)
;;; weather.el ends here
