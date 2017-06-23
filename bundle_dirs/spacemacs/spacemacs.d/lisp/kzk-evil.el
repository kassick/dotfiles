;;; kzk-evil.el --- Evil settings                    -*- lexical-binding: t; -*-

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


(setq evil-symbol-word-search t)
(setq-default evil-cross-lines t)

(define-key evil-insert-state-map (kbd "C-v") 'yank)
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; evilem on SPACE m
(evilem-default-keybindings (concat dotspacemacs-leader-key " \\" ))

;; evil embrace
(add-hook 'org-mode-hook 'embrace-org-mode-hook)
(evil-embrace-enable-evil-surround-integration)
;; MAP C-v
;; evil-args
(general-define-key "L" '(evil-forward-arg :which-key "Arg Forward")
                    "H" '(evil-backward-arg :which-key "Arg Backward")
                    "K" '(evil-jump-out-args :which-key "Jump Out of Args")
                    :states 'normal)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)


(provide 'kzk-evil)
;;; kzk-evil.el ends here
