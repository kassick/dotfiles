;;; kzk-devel.el --- Devel settings                  -*- lexical-binding: t; -*-

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




;; UNUSED

(provide 'kzk-devel)

;;; kzk-devel.el ends here

;; {{{ C / C++ support
;; (require 'google-c-style) ; google-ish style configured in my lisp directory
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; (semantic-mode t)
;;             (google-set-c-style)
;;             (google-make-newline-indent)
;;             ;; avoid company using several backends and giving headaches
;;             ;; (unless (eq major-mode 'java-mode)
;;             ;;   (set (make-local-variable 'company-backends)
;;             ;;        (quote (company-ycmd
;;             ;;                company-files
;;             ;;                company-keywords))))
;;             ))
;; }}}


;;; {{{ Java Support

;; (with-eval-after-load 'eclim
;;   (setq eclimd-autostart t))

;; }}}

;;; {{{ CSharp
;; (with-eval-after-load 'omnisharp-mode
;;   (setq omnisharp-server-executable-path
;;         (expand-file-name "~/.local/dev/ycm/third_party/ycmd/third_party/OmniSharpServer/OmniSharp/bin/Release/OmniSharp.exe"))
;;   (setq omnisharp-company-match-type 'company-match-server) ; This enables server-size flex matching
;; )
;; (eval-after-load 'company
;;      '(add-to-list 'company-backends 'company-omnisharp))

;;;}}}

;;; {{{ fsharp
;; (with-eval-after-load 'fsharp-mode
;;   (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
;;   (setq fsharp-compiler "/usr/bin/fsharpc")
;;   (setq fsharp-ac-intellisense-enabled t)
;;   (setq fsharp-ac-use-popup t))

;;(add-hook 'fsharp-mode-hook (lambda ()
;;                              (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)))
;;)

;;;}}}

;;; {{{ elixir
;;; Do not eval after load, since alchemist-server-command if const-defined on load of alchemist-server.el with alchemist-execute-command
;; (let* ((docker-run-alpine (concat "docker run"
;;                                   " --user ${UID}:${GID}"   ; run as current user
;;                                   " -v \"${HOME}:${HOME}\"" ; All home dir is available
;;                                   " -w \"$PWD\""            ; Work on CWD
;;                                   " -it"                    ; interactive, allocate tty
;;                                   " --rm"                   ; Remove container
;;                                   " elixir:alpine")))
;;   (setq alchemist-mix-command (concat docker-run-alpine " mix"))
;;   (setq alchemist-execute-command (concat docker-run-alpine " elixir"))
;;   (setq alchemist-compile-command (concat docker-run-alpine " elixirc")))
;;; }}}

;; (with-eval-after-load 'yas
;;   (yas-global-mode 1))

;; helm dash
;; (with-eval-after-load 'helm-dash
;;   (add-hook 'c-mode-hook (lambda ()
;;                            (setq-local helm-dash-docsets '("C"))))
;;   (add-hook 'c++-mode-hook (lambda ()
;;                              (setq-local helm-dash-docsets '("C" "C++"))))
;;   (add-hook 'csharp-mode-hook (lambda ()
;;                                 (setq-local helm-dash-docsets '("NET Framework"))))
;;   (setq helm-dash-docsets-path "~/.docset")
;;   (setq helm-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master")
;;   (setq helm-dash-min-length 3)
;;   (setq helm-dash-candidate-format "%d %n (%t)")
;;   (setq helm-dash-enable-debugging nil)
;;   (setq helm-dash-browser-func 'browse-url)
;;   )

;; (with-eval-after-load 'general
;;   (general-define-key :keymaps 'global
;;                       :states 'normal
;;                       :prefix dotspacemacs-leader-key
;;                       "doc" 'helm-dash)
;; )

;; ycmd setup
;; (defun ycmd-force-enable ()

;;   ;; Turn off semantic idle, as it clears the echo area from ycmd semantic info
;;   (semantic-idle-summary-mode 0)

;;   (message "Force Enable YCMD for major mode %s" major-mode)
;;   (ycmd-mode 1))

;; ;; (add-hook 'ycmd-mode-hook #'ycmd-eldoc-mode)
;; ;(add-hook 'c++-mode-hook 'ycmd-force-enable)
;; ;(add-hook 'c-mode-hook 'ycmd-force-enable)
;; ; (add-hook 'python-mode-hook 'ycmd-force-enable)

;; ;; (with-eval-after-load 'ycmd
;; ;;   (set-variable 'ycmd-global-config "~/.local/dev/ycm_conf.py")
;; ;;   (set-variable 'ycmd-server-command
;; ;;                 `("/usr/bin/python2.7"
;; ;;                   ,(expand-file-name
;; ;;                     "~/.local/dev/ycm/third_party/ycmd/ycmd")))

;;   (require 'company-ycmd)
;;   (setq company-ycmd-insert-arguments nil
;;         company-ycmd-request-sync-timeout 1.0)

;;   ;; (require 'ycmd-eldoc)

;;   (defun ycm ()
;;     (interactive)
;;     (company-cancel)
;;     (let ((ycmd-force-semantic-completion (not (company-ycmd--in-include))))
;;       (setq company-backend 'company-ycmd)
;;       (company-manual-begin)))

;;   (with-eval-after-load 'general
;;     (message "Imapping ycm")
;;     (general-define-key :keymaps 'ycmd-mode-map
;;                         :states 'insert
;;                   "<C-tab>" 'ycm))

;;   )
