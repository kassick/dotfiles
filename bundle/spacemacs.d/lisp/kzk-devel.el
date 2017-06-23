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

;; ycmd setup
(add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup)
(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'python-mode-hook 'ycmd-mode)
(set-variable 'ycmd-server-command
              `("/usr/bin/python"
                ,(expand-file-name
                  "~/.local/dev/ycm/third_party/ycmd/ycmd")))
(set-variable 'ycmd-global-config "~/.local/dev/ycm_conf.py")
(with-eval-after-load "ycmd"
  (progn
    (require 'company-ycmd)
    (setq company-ycmd-insert-arguments nil)
    (require 'ycmd-eldoc)
    (defun ycm ()
      (interactive)
      (company-cancel)
      (let ((ycmd-force-semantic-completion (not (company-ycmd--in-include))))
        (setq company-backend 'company-ycmd)
        (company-manual-begin))))

  (setq company-ycmd-request-sync-timeout 1.0)
  (general-imap :modes 'ycmd-mode "<C-tab>" 'ycm)
)


;; helm dash
  (add-hook 'c-mode-hook (lambda ()
                           (setq-local helm-dash-docsets '("C"))))
  (add-hook 'c++-mode-hook (lambda ()
                             (setq-local helm-dash-docsets '("C" "C++"))))
  (add-hook 'csharp-mode-hook (lambda ()
                                (setq-local helm-dash-docsets '("NET Framework"))))
  (setq helm-dash-docsets-path "~/.docset")
  (setq helm-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master")
  (setq helm-dash-min-length 3)
  (setq helm-dash-candidate-format "%d %n (%t)")
  (setq helm-dash-enable-debugging nil)
  (setq helm-dash-browser-func 'browse-url)

;; magit
(general-define-key :keymaps 'global
                    "C-x g" 'magit-status
                    "C-x M-g"  'magit-dispatch-popup)

;; python
(python-docstring-install)
(defun python-sort-completions (candidates)
      (defun py-is-priv (c)
        (equal (substring c 0 1) "_"))
      (defun my-filter (condp lst)
        (delq nil
              (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
      (let* ((public (my-filter (lambda (c) (not (py-is-priv c))) candidates))
             (priv (my-filter 'py-is-priv candidates)))
        (append public priv)
      )
      )

(add-hook 'python-mode-hook
              (lambda ()
                (setq py-auto-fill-mode t
                      py-comment-auto-fill-p t
                      ;; py-complete-function nil
                      )
                ;; capf with python is kind of problematic, as it sometimes sends things to the python shell and hangs up
                ;; (make-local-variable 'company-backends)
                ;; (make-local-variable 'company-transformers)
                ;; (setq company-transformers '(company-sort-by-backend-importance python-sort-completions))
                ;; (setq company-backends
                ;;       (remove-if (lambda (k) (or (and (listp k) (member 'company-capf k))
                ;;                                  (eq k 'company-capf)))
                ;;                  company-backends))
                )
              )

;;     }}}

;; {{{ C / C++ support
(require 'google-c-style) ; google-ish style configured in my lisp directory
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; (semantic-mode t)
            (google-set-c-style)
            (google-make-newline-indent)
            ;; avoid company using several backends and giving headaches
            ;; (unless (eq major-mode 'java-mode)
            ;;   (set (make-local-variable 'company-backends)
            ;;        (quote (company-ycmd
            ;;                company-files
            ;;                company-keywords))))
            ))
;; }}}


;;; {{{ Java Support

(setq eclimd-autostart t)

;; }}}

;;; {{{ CSharp
(setq omnisharp-server-executable-path
      (expand-file-name "~/.local/dev/ycm/third_party/ycmd/third_party/OmniSharpServer/OmniSharp/bin/Release/OmniSharp.exe"))
(setq omnisharp-company-match-type 'company-match-server) ; This enables server-size flex matching
;; (eval-after-load 'company
;;      '(add-to-list 'company-backends 'company-omnisharp))

;;;}}}

;;; {{{ fsharp
(setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/bin/fsharpc")
(setq fsharp-ac-intellisense-enabled t)
(setq fsharp-ac-use-popup t)
;;(add-hook 'fsharp-mode-hook (lambda ()
;;                              (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)))
;;)

;;;}}}

(yas-global-mode 1)



(provide 'kzk-devel)
;;; kzk-devel.el ends here
