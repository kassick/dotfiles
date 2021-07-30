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


;; helm dash
(with-eval-after-load 'helm-dash
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
  )

;; (with-eval-after-load 'general
;;   (general-define-key :keymaps 'global
;;                       :states 'normal
;;                       :prefix dotspacemacs-leader-key
;;                       "doc" 'helm-dash)
;; )

;; magit
(with-eval-after-load 'general
  (general-define-key :keymaps 'global
                      "C-x g" 'magit-status
                      "C-x M-g"  'magit-dispatch-popup))

;; {{{ python


;; Extract value to variable!
(defun kzk/py-refactor-extract-arg (prefix new-name &rest)
  "Extracts an argument to a variable immediately above the current statement"
  (interactive "P\nsVariable Name: ")

  (let* ((bounds (if (region-active-p)
                     (car (region-bounds)) ;; no support for non contiguous regions ...
                   (let* ((inner-arg-bounds (evil-inner-arg))
                          (start (evil-range-beginning inner-arg-bounds))
                          (end (evil-range-end inner-arg-bounds)))
                     (cons start end))))
         (start (car bounds))
         (end (cdr bounds))
         (text (buffer-substring-no-properties start end))
         (start-of-arg-marker (make-marker))
         (cur-statement-end-marker (make-marker))
         )
    ;; Save start of current argument position, we'll move the cursor to this
    ;; position later
    (set-marker start-of-arg-marker start)

    ;; Substitute the current argument text with the provided variable name
    (replace-region-contents start end (lambda () new-name))

    ;; Figure current statement's boundaries
    (let ((statement-start (save-excursion
                             (python-nav-beginning-of-statement)
                             (point)))
          (statement-end (save-excursion
                           (python-nav-end-of-statement)
                           (point))))

      ;; Leave a mark at the end of the current function call
      (set-marker cur-statement-end-marker statement-end)

      ;; Insert new variable atribution before
      (goto-char statement-start)
      (insert (concat new-name " = " text))
      (newline-and-indent)

      ;; Indent from the point there whe new statement was inserted up to the
      ;; end of the function call where the refactor was called
      (indent-region statement-start (marker-position cur-statement-end-marker)))

    ;; Place the cursor at the start of the argument that was extracted
    (goto-char (marker-position start-of-arg-marker))

    ;; clear markers
    (set-marker start-of-arg-marker nil)
    (set-marker cur-statement-end-marker nil)))

(with-eval-after-load 'bind-map
  ;;; spacemacs bind needs bind-map, which is not loaded at the time
  ;;; this code is called.
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "re" 'kzk/py-refactor-extract-arg))

(with-eval-after-load 'smartparens-python
  ;; disable this annoying behaviour, since pressing colon does not jump over
  ;; the included colon
  (setq sp-python-insert-colon-in-function-definitions nil))

(add-hook 'python-mode-hook
          (lambda ()
            (filladapt-mode t)
            (display-fill-column-indicator-mode t)))

;;     }}}

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

(with-eval-after-load 'elec-pair
  (setq electric-pair-open-newline-between-pairs nil))

(with-eval-after-load 'lsp-mode
  (message "setting lsp imenu index function")
  (setq lsp-imenu-index-function 'lsp-imenu-create-categorized-index)

  (with-eval-after-load 'general
    (message "Setting lsp-mode custom keys")
    (general-define-key :keymaps 'lsp-mode-map
                        "C-c C-h" 'lsp-ui-doc-glance
                        "C-c h" (lambda ()
                                  (interactive)
                                  (let ((help-window-select nil))
                                    (lsp-describe-thing-at-point))))))

(spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)

(add-hook 'prog-mode-hook (lambda ()
                            (message "enabling truncate lines for a prog buffer")
                            (make-variable-buffer-local 'toggle-truncate-lines)
                            (setq truncate-lines t)))


(provide 'kzk-devel)
;;; kzk-devel.el ends here
