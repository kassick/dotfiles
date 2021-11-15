;;; kzk-shell-hacks.el --- Hacks for eshell, ansi-term and term  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodrigo Kassick

;; Author: Rodrigo Kassick <kassick@Voyager>
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

;; This loads eshell aliases on startup -- useful to helm eshell-command-on-file
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (let ((default-directory (getenv "HOME")))
;;               (command-execute 'eshell)
;;               (bury-buffer))))


;; Eshell settings
;; (setq eshell-destroy-buffer-when-process-dies t)
;; (defun eshell-maybe-bol ()
;;   "Go to beginning of the input line or the first char"
;;       (interactive)
;;       (let ((p (point)))
;;         (eshell-bol)
;;         (if (= p (point))
;;             (beginning-of-line))))

;; (eval-after-load 'eshell
;;   (lambda ()
;;     (when (not (boundp 'eshell-visual-subcommands))
;;       (setq eshell-visual-subcommands nil))
;;     (push '("git" "log" "diff" "show") eshell-visual-subcommands)
;;     (when (not (boundp 'eshell-visual-commands))
;;       (setq eshell-visual-commands nil))
;;     (push '("less") eshell-visual-commands)
;;     ;;(add-to-list 'eshell-visual-commands "vim")

;;     ;; Map bol to eshell-maybe-bol in normal, insert and visual
;;     (evil-define-key 'normal eshell-mode-map
;;              (kbd "C-a") 'eshell-maybe-bol
;;              (kbd "<home>") 'eshell-maybe-bol
;;              (kbd "C-a") 'eshell-maybe-bol)
;;     (evil-define-key 'insert eshell-mode-map
;;              (kbd "<home>") 'eshell-maybe-bol)
;;     (evil-define-key 'visual 'eshell-mode-map
;;              (kbd "C-a") 'eshell-maybe-bol
;;              (kbd "<home>") 'eshell-maybe-bol)
;;     ))

;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (define-key eshell-mode-map (kbd "<tab>") 'company-manual-begin)
;;             (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)
;;             (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol)
;;             ))

;; ;; Shell mode / ansi-shell
;; (eval-after-load 'evil
;;   (lambda ()
;;     (evil-set-initial-state 'term-mode 'emacs)
;;     ))

;; ;; Make tab completion use readline on term-mode (not ansi-term)
;; (use-package readline-complete
;;   :ensure t
;;   :after company
;;   :init
;;   (setq explicit-shell-file-name "bash")
;;   (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;;   (setq comint-process-echoes t)
;;   :config
;;   (push 'company-readline company-backends)
;;   (define-key shell-mode-map (kbd "<tab>") 'company-manual-begin)
;;   (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
;;   )


;; makes sure that process buffers are killed after the process finished
(require 'dying)
(unless (fboundp 'start-process-orig)
  (fset 'start-process-orig (symbol-function 'start-process)))

(fset 'start-process
      (append
       (list 'lambda
	     '(name buffer program &rest program-args)
	     (concat (let ((s (documentation 'start-process))) (substring s 0 (string-match "\n\n.*\\'" s)))
	       "\nAfter finishing the process the process buffer goes into dying-mode."))
       '((let ((ret (apply 'start-process-orig name buffer program program-args)))
	   (if ret
	       (set-process-sentinel ret
				     '(lambda (proc event)
					(when (and (eq (process-status proc) 'exit)
						   (process-buffer proc))
					  (with-current-buffer (process-buffer proc)
					    (dying-mode t)))))) ret))))

(defadvice shell-command-sentinel (after shell-command-sentinel-dying-mode (proc event) activate)
  "The original shell-command-sentinel prevents insertion of a termination message into the process buffer itself.
The advice starts dying-mode after the process exits."
  (when (and (eq (process-status proc) 'exit)
	     (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (dying-mode t))))

;; Do not pop up the output of an async thell command
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(provide 'kzk-shell)
;;; kzk-shell-hacks.el ends here
