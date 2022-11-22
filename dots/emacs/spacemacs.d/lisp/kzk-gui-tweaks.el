;;; kzk-gui-tweaks.el --- Tweaks to the GUI          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodrigo Kassick

;; Author: Rodrigo Kassick <kassick@voyager>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; {{{
;; this hack does not seem necessary any longer
(defun kzk/set-default-face (&optional frame)
  (message "Readjusting font for new frame %S" (if frame frame "before"))
  (let* ((frame-font (frame-parameter frame 'font)))
    (message "Readjusting frame font: %S" frame-font)
    (set-face-attribute 'default frame :font frame-font))
    "Frame font set"
  )

(defun kzk/set-frame-font (&optional frame)
  (run-at-time "3 seconds" nil (lambda () (kzk/set-default-face frame)))
  (remove-hook 'after-make-frame-functions #'kzk/set-frame-font)
  "Font will be adjusted Adjusted"
  )

;; (add-hook 'after-make-frame-functions #'kzk/set-frame-font)
;; }}}


(defun kzk/frames-minibuffer-window ()
  (mapcar (lambda (frame)
            (let ((frame-window (minibuffer-window frame))
                  (frame-minibuffer (frame-parameter frame 'minibuffer))
                  (frame-name (frame-parameter frame 'name)))
              (message "Frame `%s' (%S) %s minibuffer window %S"
                       (if (string-empty-p frame-name) "!!! untitled !!!" frame-name)
                       frame
                       (cond
                        ((eq t frame-minibuffer) "has own")
                        ((eq 'only frame-minibuffer) "is only a")
                        ((windowp frame-minibuffer) "shares"))
                       frame-window)))
          (frame-list))
  )


(provide 'kzk-gui-tweaks)
