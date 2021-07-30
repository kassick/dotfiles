;;; kzk-evil-init.el --- Initialization for evil     -*- lexical-binding: t; -*-

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

;; {{{ Give me bach universal argument.
;; <leader>u on evil, C-u on helm,, etc -- too confusing. Just stick with emacs global C-u binding everywhere
(setq evil-want-C-u-scroll nil)

;; Setting the key is unecessary, since want-C-u is nil
;; (general-define-key :states '(normal visual motion insert)
;;                     "C-u" 'universal-argument)

;; Still must manually bind it on evilified-state-map
(with-eval-after-load 'general
  (general-define-key :keymaps '(evil-evilified-state-map-original evil-evilified-state-map)
                      "C-u" 'universal-argument))
;; }}}

(setq evil-symbol-word-search t)
(setq evil-cross-lines t)

(with-eval-after-load 'imenu-list
  (push 'imenu-list-major-mode evil-emacs-state-modes))


(provide 'kzk-evil-init)
;;; kzk-evil-init.el ends here
