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

;; from Manuel Uberti's dot-emacs
(defun kzk/setup-main-fonts (default-height variable-pitch-height)
  "Set up default fonts.

Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."

  (set-face-attribute 'default nil
                      :family kzk/default-font
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family kzk/variable-pitch-font
                      :height variable-pitch-height
                      :weight 'regular))

(defun kzk/adjust-font-size ()
  ;; Dinamically change font size based upon screen resolution
  (if (display-graphic-p)
      (if (> (display-pixel-width) 1800) ; Has X, query pixel width
          (kzk/setup-main-fonts kzk/display-width/>1800/default-font-height
                                kzk/display-width/>1800/variablepitch-height)
        (kzk/setup-main-fonts kzk/display-width/<1800/default-font-height
                              kzk/display-width/<1800/variablepitch-height))
    ;; no X yet, maybe we are a server starting?
    (kzk/setup-main-fonts kzk/display-width/>1800/default-font-height
                          kzk/display-width/>1800/variablepitch-height)
    )
  )

(add-hook 'before-make-frame-hook 'kzk/adjust-font-size)
(kzk/adjust-font-size)

(provide 'kzk-gui-tweaks)
;;; kzk-gui-tweaks.el ends here
