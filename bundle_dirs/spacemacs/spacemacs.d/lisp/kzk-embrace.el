;;; kzk-embrace.el --- Kludge to fix embrace.el / evil-embrace.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodrigo Kassick

;; Author: Rodrigo Kassick <kassick@antagorda>
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

;; This is a kludge. Issues involved:
;; - https://github.com/cute-jumper/evil-embrace.el/issues/5

;;; Code:

(setq kzk/embrace--embrace-help-window nil)

(defun kzk/embrace-show-help-p ()
  "Either evil-embrace will set show-help-p or embrace will"
  (or (and (boundp 'evil-embrace-show-help-p) evil-embrace-show-help-p)
      (and (boundp 'embrace-show-help-p) embrace-show-help-p)))

(defun kzk/embrace--store-help-window (&rest args)
  "Set internal variable to the window used by embrace--show-help-buffer"
  (setq kzk/embrace--embrace-help-window
        (if (kzk/embrace-show-help-p)
            (get-buffer-window embrace--help-buffer)
          nil))
  )

(defun kzk/embrace--kill-help-window (&rest args)
  "If the internal window variable is set, delete it"
  (when (and kzk/embrace--embrace-help-window
           (window-valid-p kzk/embrace--embrace-help-window)
           (not (eq (selected-window) kzk/embrace--embrace-help-window)))
      (delete-window kzk/embrace--embrace-help-window)
    )

  (setq kzk/embrace--embrace-help-window nil))

(defun kzk/add-advise-kill-surround-help-window (&rest args)
  ;; KLUDGE: Store help window in a internal variable after calling embrace--show-help-buffer . Kill it after any action of evil surround
  (advice-add 'embrace--show-help-buffer :after 'kzk/embrace--store-help-window)
  (advice-add 'evil-surround-region :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-surround-change :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-surround-delete :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-surround-edit :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-Surround-edit :after 'kzk/embrace--kill-help-window)
  )

(defun kzk/del-advise-kill-surround-help-window (&rest args)
  (advice-remove 'embrace--show-help-buffer 'kzk/embrace--store-help-window)
  (advice-remove 'evil-surround-region 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-surround-change 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-surround-delete 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-surround-edit 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-Surround-edit 'kzk/embrace--kill-help-window)
  )

;; After evil-embrace : Advise the surround integration functions to create/remove the advices
(with-eval-after-load 'evil-embrace
  (advice-add 'evil-embrace-enable-evil-surround-integration
              :after 'kzk/add-advise-kill-surround-help-window)
  (advice-add 'evil-embrace-disable-evil-surround-integration
              :after 'kzk/del-advise-kill-surround-help-window)
  )

(provide 'kzk-embrace)
;;; kzk-embrace.el ends here
