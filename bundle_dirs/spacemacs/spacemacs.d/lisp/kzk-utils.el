;;; kzk-utils.el --- Utility functions               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Rodrigo Kassick

;; Author: Rodrigo Kassick <rodrigokassick@mckzk>

;; @TODO: make this generic depending on platform
(defun kzk/open-fun (fname)
  (save-window-excursion
    (let ((process-connection-type nil)
          (fname-for-cmd (shell-quote-argument (expand-file-name fname)))
          (cmd (cond
                ((eq system-type 'darwin) "open")
                ((eq system-type 'gnu/linux) "xdg-open")
                (t "open"))))
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


(defun kzk/insert-time-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
                       two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond ((not prefix) "%FT%T%z")
                      ((equal prefix '(4)) "[%H:%M:%S; %Y-%m-%d]")
                      (t "[%H:%M:%S; %d.%m.%Y]"))))
    (insert (format-time-string format))))

(provide 'kzk-utils)
