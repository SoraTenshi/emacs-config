;;; system-stats.el --- Small system resource monitor for emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Shows a simple resource monitor.
;;; Code:

(defconst stats-buffer-name "*System Stats*"
  "Buffer name for system stats.")

(defvar stats--interval nil
  "Interval for refreshes on the system stats.")

(defvar stats--timer nil
  "Timer object for the update loop.")

(defvar stats--pending-updates 0
  "Number of pending async updates.")

(defvar stats--cpu-data "")
(defvar stats--mem-data "")

(defun stats--format-bytes (bytes)
  "Format BYTES into a human readable string."
  (let* ((kb 1000.0)
         (mb (* kb 1000.0))
         (gb (* mb 1000.0))
         (tb (* gb 1000.0)))
    (cond
     ((>= bytes tb) (format "%.2f TB" (/ bytes tb)))
     ((>= bytes gb) (format "%.2f GB" (/ bytes gb)))
     ((>= bytes mb) (format "%.2f MB" (/ bytes mb)))
     ((>= bytes kb) (format "%.2f KB" (/ bytes kb)))
     (t (format "%d B" bytes)))))

(defun stats--parse-cpu (raw-output)
  "Parse CPU stats from RAW-OUTPUT and return formatted string with per-core and total."
  (let ((lines (split-string (string-trim raw-output) "\n" t))
        (core-stats '())
        (sum 0.0)
        (count 0))
    (dolist (line lines)
      (when (string-match "\\([0-9.]+\\)" line)
        (let ((val (string-to-number (match-string 1 line))))
          (push (format "Core %d - %.1f%%" count val) core-stats)
          (setq sum (+ sum val))
          (setq count (1+ count)))))
    (setq core-stats (nreverse core-stats))
    (when (> count 0)
      (push (format "Total - %.1f%%" (/ sum count)) core-stats))
    (string-join core-stats "\n  ")))

(defun stats--parse-mem (raw-output)
  "Parse memory percentage from RAW-OUTPUT and return formatted string."
  (format "%.1f%% Used" (string-to-number (string-trim raw-output))))

(defun stats--fetch-cpu-linux ()
  "Fetch CPU stats on Linux."
  (let ((cpu-buffer (generate-new-buffer " *system-stats-cpu*")))
    (set-process-sentinel
     (start-process "system-stats-cpu" cpu-buffer
                    "awk '/^cpu[0-9]/ {print ($2+$4)*100/($2+$4+$5)}' /proc/stat")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (stats--update-result 'cpu (stats--parse-cpu output))))))))

(defun stats--fetch-cpu-windows ()
  "Fetch CPU stats on Windows."
  (let ((cpu-buffer (generate-new-buffer " *system-stats-cpu*")))
    (set-process-sentinel
     (start-process "system-stats-cpu" cpu-buffer
                    "powershell" "-NoProfile" "-Command"
                    "(Get-Counter '\\Processor(*)\\% Processor Time').CounterSamples | Where-Object {$_.InstanceName -match '^[0-9]+$'} | Sort-Object {[int]$_.InstanceName} | ForEach-Object {$_.CookedValue}")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (stats--update-result 'cpu (stats--parse-cpu output))))))))

(defun stats--fetch-cpu-darwin ()
  "Fetch CPU stats on macOS."
  (let ((cpu-buffer (generate-new-buffer " *system-stats-cpu*")))
    (set-process-sentinel
     (start-process "system-stats-cpu" cpu-buffer
                    "top -l 1 | grep 'CPU usage'")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (stats--update-result 'cpu (string-trim output))))))))

(defun stats--fetch-mem-linux ()
  "Fetch memory stats on Linux."
  (let ((mem-buffer (generate-new-buffer " *system-stats-mem*")))
    (set-process-sentinel
     (start-process "system-stats-mem" mem-buffer
                    "free | awk '/Mem:/ {print $3/$2*100}'")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (stats--update-result 'mem (stats--parse-mem output))))))))

(defun stats--fetch-mem-windows ()
  "Fetch memory stats on Windows."
  (let ((mem-buffer (generate-new-buffer " *system-stats-mem*")))
    (set-process-sentinel
     (start-process "system-stats-mem" mem-buffer
                    "powershell" "-Command"
                    "$m=Get-CimInstance Win32_OperatingSystem; (($m.TotalVisibleMemorySize-$m.FreePhysicalMemory)/$m.TotalVisibleMemorySize)*100")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (stats--update-result 'mem (stats--parse-mem output))))))))

(defun stats--fetch-mem-darwin ()
  "Fetch memory stats on macOS."
  (let ((mem-buffer (generate-new-buffer " *system-stats-mem*")))
    (set-process-sentinel
     (start-process "system-stats-mem" mem-buffer
                    "top -l 1 | awk '/PhysMem/ {print \"RAM - \" $2 \" used\"}'")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (stats--update-result 'mem (string-trim output))))))))

(defun stats--fetch-cpu ()
  "Fetch CPU stats based on current OS."
  (pcase system-type
    ('gnu/linux (stats--fetch-cpu-linux))
    ('windows-nt (stats--fetch-cpu-windows))
    ('darwin (stats--fetch-cpu-darwin))))

(defun stats--fetch-mem ()
  "Fetch memory stats based on current OS."
  (pcase system-type
    ('gnu/linux (stats--fetch-mem-linux))
    ('windows-nt (stats--fetch-mem-windows))
    ('darwin (stats--fetch-mem-darwin))))

(defun stats--fetch-all ()
  "Asynchronously fetch all system stats and update buffer."
  (let ((buffer (get-buffer stats-buffer-name)))
    (unless buffer
      (cl-return-from stats--fetch-all))

    (setq stats--pending-updates 2)
    (stats--fetch-cpu)
    (stats--fetch-mem)))

(defun stats--update-result (type data)
  "Update result for TYPE with DATA and refresh display if all results are in."
  (pcase type
    ('cpu (setq stats--cpu-data data))
    ('mem (setq stats--mem-data data)))

  (setq stats--pending-updates
        (max 0 (1- stats--pending-updates)))

  (when (= stats--pending-updates 0)
    (stats--render-buffer)))

(defun stats--render-buffer ()
  "Render the stats buffer with current data."
  (let ((buf (get-buffer stats-buffer-name)))
    (when (and buf (get-buffer-window buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "======System Monitor======

CPU:
  %s

Memory:
  %s

Last Updated: %s"
                          stats--cpu-data
                          stats--mem-data
                          (format-time-string "%H:%M:%S")))
          (goto-char (point-min))
          (set-buffer-modified-p nil))))))

(defun stats--update-buffer ()
  "Update the stats buffer if it is currently visible."
  (unless (get-buffer stats-buffer-name)
    (when stats--timer
      (cancel-timer stats--timer)
      (setq stats--timer nil))
    (cl-return-from stats--update-buffer))

  (when (get-buffer-window stats-buffer-name)
    (stats--fetch-all)))

(defun stats/toggle (interval)
  "Toggle the system stats monitor.
INTERVAL specifies the update interval in seconds."
  (interactive "nUpdate interval (seconds): ")
  (setq stats--interval interval)
  (if (get-buffer-window stats-buffer-name)
      (kill-buffer stats-buffer-name)
    (let ((buf (get-buffer-create stats-buffer-name)))
      (switch-to-buffer buf)
      (special-mode)
      (setq stats--cpu-data "Fetching...")
      (setq stats--mem-data "Fetching...")
      (stats--render-buffer)
      (stats--fetch-all)
      (when stats--timer
        (cancel-timer stats--timer))
      (setq stats--timer
            (run-with-timer stats--interval
                            stats--interval
                            #'stats--update-buffer)))))

(defun stats/freeze ()
  "Freezes the stat buffer."
  (interactive)
  (when (and stats--timer
             (equal (buffer-name) stats-buffer-name))
    (cancel-timer stats--timer)
    (setq stats--timer nil)))

(defun stats/unfreeze ()
  "Unfreezes the stat buffer."
  (interactive)
  (when (and (not stats--timer)
             (equal (buffer-name) stats-buffer-name))
    (setq stats--timer
          (run-with-timer stats--interval
                          stats--interval
                          #'stats--update-buffer))))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and stats--timer
                       (equal (buffer-name) stats-buffer-name))
              (cancel-timer stats--timer)
              (setq stats--timer nil))))

(provide 'system-stats)
;;; system-stats.el ends here
