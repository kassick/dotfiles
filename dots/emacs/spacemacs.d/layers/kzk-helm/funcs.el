;;; funcs.el --- KZK Helm functions                  -*- lexical-binding: t; -*-

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

;;;;;; helm-hacks.el --- Hacks to helm                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  kassick

;; Author: kassick <kassick@cacequi.inf.ufrgs.br>
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

(defun kzk-helm/ff-insert-file-name (candidate)
  "Inserts the candidate into the buffer. With prefix, insert full path"
  (cond ((eq nil helm-current-prefix-arg )
         ;; (message "rel path %S" (file-relative-name candidate))
         (insert (file-relative-name candidate)))
        ((not (null helm-current-prefix-arg))
         (insert (expand-file-name candidate)))
        (t
         (insert candidate))))

(defun kzk-helm/run-ff-insert-file-name ()
  "Inserts selected file name at point"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'kzk-helm/ff-insert-file-name)))

(defun kzk/helm-ff-hacks-setup ()

  ;; Bind C-c i to insert file name and add it to the find file action list
  ;; helm-ff actions
  (add-to-list 'helm-find-files-actions '("Insert file name at point `C-c C-i'" . kzk-helm/ff-insert-file-name) t)
  (define-key helm-find-files-map (kbd "C-c C-i") 'kzk-helm/run-ff-insert-file-name)
  ;; helm-type-file (apparently, ff does not inherit typefile actions)
  (add-to-list 'helm-type-file-actions '("Insert file name at point `C-c C-i'" . kzk-helm/ff-insert-file-name) t)
  (define-key helm-generic-files-map (kbd "C-c C-i") 'kzk-helm/run-ff-insert-file-name))

(provide 'funcs)
;;; funcs.el ends here
