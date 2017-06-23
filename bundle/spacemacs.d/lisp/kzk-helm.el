;;; kzk-helm.el --- Helm settings                    -*- lexical-binding: t; -*-

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

  ;; Helm

  ;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
  (setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-echo-input-in-header-line t
        ;helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)

  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)

  ;; Helm find files
  (setq helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-file-name-history-use-recentf t
        hhelm-ff-fuzzy-matching  nil)

  (require 'kzk-helm-hacks)
  ;; Helm-M-x
  (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

  ;; Helm-mini
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  (general-define-key :prefix dotspacemacs-leader-key :states '(normal visual motion)
                      "h a"  '(helm-ag :which-key "Ag (cwd)"))


;; Helm flyspell
(general-define-key :keymaps 'flyspell-mode-map
                    "C-;" 'helm-flyspell-correct)

(general-define-key :keymaps 'flyspell-mode-map
                    :prefix dotspacemacs-leader-key
                    :modes '(normal motion visual)
                    "S ;" 'helm-flyspell-correct)



(provide 'kzk-helm)
;;; kzk-helm.el ends here
