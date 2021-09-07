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
;; (use-package dedicated :ensure t)
;; (general-define-key :keymaps 'global
;;                     "C-x 9" 'dedicated-mode)

;; narrow indirect
;; (general-define-key :keymaps 'ctl-x-4-map
;;               "nd" 'ni-narrow-to-defun-indirect-other-window
;;               "nn" 'ni-narrow-to-region-indirect-other-window
;;               "np" 'ni-narrow-to-page-indirect-other-window)

(general-define-key :keymaps 'spacemacs-cmds
                    "w _" 'evil-window-set-height
                    "w |" 'evil-window-set-width)

;; {{{ imenu-list
(general-define-key :keymaps    'global
                    "<C-f12>"   'imenu-list-smart-toggle)

(with-eval-after-load 'imenu-list
  (add-hook 'imenu-list-major-mode-hook 'dedicated-mode))
;; }}}

;; Directly copied from frame.el but now hide Emacs instead of killing
;; it when last frame will be closed.
;; (defun handle-delete-frame-without-kill-emacs (event)
;;   "Handle delete-frame events from the X server."
;;   (interactive "e")
;;   (let ((frame (posn-window (event-start event)))
;;         (i 0)
;;         (tail (frame-list)))
;;     (while tail
;;       (and (frame-visible-p (car tail))
;;            (not (eq (car tail) frame))
;;            (setq i (1+ i)))
;;       (setq tail (cdr tail)))
;;     (if (> i 0)
;;         (delete-frame frame t)
;;       ;; Not (save-buffers-kill-emacs) but instead:
;;       (ns-do-hide-emacs))))

;; (when (eq system-type 'darwin)
;;   (advice-add 'handle-delete-frame :override
;;               #'handle-delete-frame-without-kill-emacs)

(defun kzk/beginning-of-visual-line-or-indent (&optional n)
  "Move point to:
  - beginning of current visual line
  - indent
  - first character
  "
  (interactive "^p")
  (or n (setq n 1))
  (let ((opoint (point)))
    (when (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
    (vertical-motion 0)
    ;; Constrain to field boundaries, like `move-beginning-of-line'.
    (goto-char (constrain-to-field (point) opoint (/= n 1)))

    (when (= opoint (point))
      (back-to-indentation))

    (when (= opoint (point))
      (move-beginning-of-line 1))))

(defun kzk/beginning-of-visual-line-or-indent-v2 (&optional n)
  "Move point to:
  - beginning of current visual line
  - indent
  - first character
  "
  (interactive "^p")
  (or n (setq n 1))
  (let ((opoint (point)))
    (when (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))

    (defun point-or-bail ()
      (if (= (point) opoint)
          -1
        (point))
      )

    (let ((target
           (save-excursion
             (max (progn
                    (vertical-motion 0)
                    (goto-char (constrain-to-field (point) opoint (/= n 1)))

                    (point-or-bail))
                  (progn
                    (back-to-indentation)
                    (point-or-bail))

                  (progn
                    (move-beginning-of-line 1)
                    (point-or-bail))
           ))))

      (if (/= target -1)
          (goto-char target)))))

(defun kzk/end-of-visual-line-or-eol (&optional n)
  "Move point to :
  - end of visual line
  - end of line
  "
  (interactive "^p")
  (or n (setq n 1))
  (let ((opoint (point)))
    (when (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
    ;; Unlike `move-beginning-of-line', `move-end-of-line' doesn't
    ;; constrain to field boundaries, so we don't either.
    (vertical-motion (cons (window-width) 0))

    (when (= opoint (point))
      (end-of-line 1))))


(global-set-key (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent-v2)
(global-set-key (kbd "<end>") 'kzk/end-of-visual-line-or-eol)

(with-eval-after-load 'general
  (general-define-key :states '(normal visual motion insert)
                      "<home>" 'kzk/beginning-of-visual-line-or-indent-v2
                      "<end>" 'kzk/end-of-visual-line-or-eol))

;; )


(provide 'kzk-window-management)
;;; kzk-window-management.el ends here
