;;; packages.el --- Overrides of spacemacs-visual-editing layer  -*- lexical-binding: t; -*-

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


(setq kzk-visual-editing-packages
      '(
        ;; default
        adaptive-wrap))

(defun kzk-visual-editing/init-adaptive-wrap ()
  ;; Adaptive wrap: do not load it with the layers; load it here so we can tweak the visual-line-mode-hook function: DO NOT MESS WITH MY ORG

  (use-package adaptive-wrap :ensure t)

  (defun kzk/adaptive-wrap-maybe ()
    (if (not (eq major-mode 'org-mode))
        (adaptive-wrap-prefix-mode t)
      )
    )

  (let* ((tmp-hooks (cl-remove-if
                     (lambda (itm)
                       (eq itm 'adaptive-wrap-prefix-mode))
                     visual-line-mode-hook))
          (new-hooks (append
                      tmp-hooks
                      '(kzk/adaptive-wrap-maybe))))
    (setq visual-line-mode-hook new-hooks))

  ;; end
  )

;;; packages.el ends here
