;;; kzk-latex.el ---                                 -*- lexical-binding: t; -*-

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

(with-eval-after-load 'tex
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-command-default "LatexMk"
        )

  ;; Stop fontifying super/subscript with :height 0.85 , it becomes really
  ;; ANNOYING when editting something in an aligned environment, such as nd and
  ;; others
  (with-eval-after-load 'font-latex
   (set-face-attribute 'font-latex-subscript-face nil :height 1.0)
   (set-face-attribute 'font-latex-superscript-face nil :height 1.0)
   )

  (with-eval-after-load 'general
    (general-define-key :keymaps 'LaTeX-mode-map
                        :states '(normal motion visual)
                        :prefix dotspacemacs-leader-key
                        "l" '(nil :which-key "LaTeX")
                        "ll" '(compile :which-key "Compile Document")
                        "lv" '(TeX-view :which-key "View Document"))
    (general-define-key :keymaps 'LaTeX-mode-map
                        :states 'insert
                        :prefix nil
                        (general-chord "[[") 'LaTeX-environment
                        (general-chord "]]") 'LaTeX-close-environment
                        )
    )
  )

(provide 'kzk-latex)
;;; kzk-latex.el ends here
