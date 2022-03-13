;;; kzk-utils.el --- Utility functions               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Rodrigo Kassick

;; Author: Rodrigo Kassick <rodrigokassick@mckzk>

(defcustom kzk/open-externally-command (cond
                                        ((eq system-type 'darwin) "open")
                                        ((eq system-type 'gnu/linux) "xdg-open")
                                        (t "open"))
  "Command used to open a file externally. Used by kzk/open-fun. Default: open or xdg-open, dependent on the system")

(defun kzk/open-fun (fname)
  (save-window-excursion
    (let ((process-connection-type nil)
          (fname-for-cmd (shell-quote-argument (expand-file-name fname)))
          (cmd kzk/open-externally-command))
      (message "Running %s for %s" cmd fname-for-cmd)
      (start-process-shell-command (format "%s '%s'" cmd fname-for-cmd)
                                   nil
                                   (format "%s '%s'" cmd fname-for-cmd)))))

(defun kzk/insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
                       two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "[%Y-%m-%d]"))))
    (insert (format-time-string format))))

(defun kzk/iso-date ()
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))


(defun kzk/insert-time-date (prefix)
  "Insert the current date. No Prefix: ISO; Prefix=4 for org-timestamp with Y-M-d; otherwise org-timestamp d-M-Y"
  (interactive "P")
  (let ((date-str (cond
                  ((not prefix) (kzk/iso-date) )
                  ((equal prefix '(4))  (format-time-string "[%H:%M:%S; %Y-%m-%d]"))
                  (t (format-time-string "[%H:%M:%S; %d.%m.%Y]")))))
    (insert date-str)))


(defmacro kzk/after-init (&rest FORMS)
  `(add-hook (quote after-init-hook) (lambda () ,@FORMS)))


(provide 'kzk-utils)
