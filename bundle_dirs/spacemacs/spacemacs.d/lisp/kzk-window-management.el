;;; kzk-window-management.el --- Window Management   -*- lexical-binding: t; -*-

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
;;; Define conveninent vim-like switch between splited windows

;;; Code:


;; Global keybindings
(message "Loading kzk-window-management")
;;; {{{ Switch buffers
(general-define-key :keymaps    'global
                    "C-x b"     'helm-mini
                    "C-x C-b"   'helm-mini
                    "<C-f10>"   'ibuffer
                    "<C-S-f10>" 'ibuffer-other-window)
;;; }}}

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x p") 'evil-window-mru)

(global-set-key (kbd "C-x C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x C-<down>") 'shrink-window)
(global-set-key (kbd "C-x C-<up>") 'enlarge-window)

(general-define-key :keymaps 'global
                    "C-x 7 2" 'esw/select-window
                    "C-x 7 s" 'esw/select-window
                    "C-x 7 m" 'esw/move-window
                    "C-x 7 b" 'esw/show-buffer
                    "C-x 7 C-s" 'esw/swap-two-windows
                    "C-x 7 0" 'esw/delete-window)

;; popwin
  (push '("\\*company-documentation\\*" :height 10 :position bottom :noselect t)
        popwin:special-display-config)
  (push '("^\\*Flycheck.+\\*$" :regexp t
          :dedicated t :position bottom :stick t :noselect t)
        popwin:special-display-config)
  (push '(compilation-mode :noselect t)
        popwin:special-display-config)

;; dedicated
(general-define-key :keymaps 'global
                    "C-x 9" 'dedicated-mode)

;; narrow indirect'
(general-define-key :keymaps 'ctl-x-4-map
              "nd" 'ni-narrow-to-defun-indirect-other-window
              "nn" 'ni-narrow-to-region-indirect-other-window
              "np" 'ni-narrow-to-page-indirect-other-window)

(general-define-key :keymaps 'spacemacs-cmds
                    "w _" 'evil-window-set-height
                    "w |" 'evil-window-set-width)


(provide 'kzk-window-management)
;;; kzk-window-management.el ends here
