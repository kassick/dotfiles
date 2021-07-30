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

;;; Commentary:

;;

;;; Code:

(defun kzk-helm/ff-insert-file-name (candidate)
  "Inserts the candidate into the buffer. With prefix, insert full path"
  (message "candidate is %S args is %S" candidate helm-current-prefix-arg)
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

(defun helm-ff-run-elscreen-find-file ()
  "Run helm-elscreen-find-file on current selection"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-elscreen-find-file)))

(defun helm-buffers-run-find-buffer-on-elscreen ()
  "Run helm-find-buffer-on-elscreen"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-find-buffer-on-elscreen)))

(defun helm-esw/show-buffer (candidate)
  (set-window-buffer (esw/select-window nil t t) (get-buffer candidate)))

(defun helm-esw/find-file (candidate)
  (set-window-buffer (esw/select-window nil t t) (find-file-noselect candidate)))

(defun helm-esw/run-show-buffer ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/show-buffer)))

(defun helm-esw/run-find-file ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/find-file)))

;;(eval-after-load 'helm-files
(defun kzk/helm-ff-hacks-setup ()
    ;;(if (locate-library "elscreen")
        ;;(progn
          ;;;; Add the shortcut description
          ;;(setf (car (rassoc 'helm-elscreen-find-file helm-find-files-actions))
                ;;"Find file in Elscreen `C-c C-z'")
          ;;;; Bind C-c C-z
          ;;(define-key helm-find-files-map (kbd "C-c C-z") 'helm-ff-run-elscreen-find-file)))

  ;; Bind C-c i to insert file name and add it to the find file action list
  (add-to-list 'helm-find-files-actions
               '("Insert file name at point `C-c C-i'" . kzk-helm/ff-insert-file-name)
               t)
  (define-key helm-find-files-map (kbd "C-c C-i") 'kzk-helm/run-ff-insert-file-name)

    (if (locate-library "es-windows")
        (progn
          ;; Add description
          (add-to-list 'helm-find-files-actions
                       '("Find file in in new splited window `C-c w'" . helm-esw/find-file ) t)

          ;; Bind C-c C-w
          (define-key helm-find-files-map (kbd "C-c w") 'helm-esw/run-find-file))))

;;(eval-after-load 'helm-buffers
(defun kzk/helm-buffers-hacks-setup ()
    ;; (if (locate-library "elscreen")
        ;; (progn
          ;; ;; Add the shortcut description
          ;; (setf (car (rassoc 'helm-find-buffer-on-elscreen helm-type-buffer-actions))
                ;; "Display Buffer in Elscreen `C-c C-z'")
          ;; (define-key helm-buffer-map (kbd "C-c C-z") 'helm-buffers-run-find-buffer-on-elscreen)))
    (if (locate-library "es-windows")
        (progn
          ;; Add description
          (add-to-list 'helm-type-buffer-actions
                       '("Display buffer(s) in new splited window `C-c w'" . helm-esw/show-buffer) t)
          ;; Bind C-c C-w
          (define-key helm-buffer-map (kbd "C-c w") 'helm-esw/run-show-buffer))))

;;(eval-after-load 'helm-projectile
(defun kzk/helm-projectile-hacks-setup ()
    ;; (if (locate-library "elscreen")
        ;; (define-key helm-projectile-find-file-map (kbd "C-c C-z") 'helm-ff-run-elscreen-find-file))
    (if (locate-library "es-windows")
        (define-key helm-projectile-find-file-map (kbd "C-c w") 'helm-esw/run-find-file)))

(provide 'funcs)
;;; funcs.el ends here
