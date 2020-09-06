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

(provide 'kzk-gui-tweaks)

(defun kzk/set-default-face (&optional frame)
  (message "Readjusting font for new frame %S" (if frame frame "before"))
  (let* ((frame-font (frame-parameter frame 'font)))
    (message "Setting frame font to \"%s\"" frame-font)
    (set-face-attribute 'default frame :font frame-font))
  ;; (catch 'break
  ;;   (dolist (cur-font dotspacemacs-default-font)
  ;;     (let* ((name (car cur-font))
  ;;            (raw-props (cdr cur-font))
  ;;            (props-no-powerline (spacemacs/mplist-remove raw-props :powerline-scale))
  ;;            (props (spacemacs/mplist-remove props-no-powerline :powerline-offset))
  ;;            (foundry (plist-get props ':foundry))
  ;;            (full-name (s-trim (concat name " " foundry))))
  ;;       (message "Trying \"%s\"" full-name)
  ;;       (when (find-font (font-spec :name full-name))
  ;;         (message "Setting default face to \"%s\"" full-name)
  ;;         (apply 'set-face-attribute
  ;;                'default nil
  ;;                :family name
  ;;                props)
  ;;         (throw 'break t))))
  ;;   (message "No valid font found :/"))
  )


    ;; ;; (spacemacs/set-default-font dotspacemacs-default-font)
    ;; (set-face-attribute 'default nil
    ;;                      :family "Fira Code"
    ;;                      :foundry "Retina"
    ;;                      :height 120
    ;;                      :weight 'normal
    ;;                      :width 'normal
    ;;                      ))

(defun kzk/set-frame-font (&optional frame)
  (run-at-time "1 seconds" nil (lambda () (kzk/set-default-face frame)))
  (remove-hook 'after-make-frame-functions #'kzk/set-frame-font)
  )

(add-hook 'after-make-frame-functions #'kzk/set-frame-font)


;;; kzk-gui-tweaks.el ends here
