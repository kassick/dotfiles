;;; kzk-company.el ---                               -*- lexical-binding: t; -*-

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
  (setq company-idle-delay 0.9
        ;; company-idle-delay 0.9
        ;; company-transformers '(company-sort-by-backend-importance)
        company-selection-wrap-around t
        company-show-numbers t
        ;; company-require-match nil
        ;; company-auto-complete #'kzk/company-visible-and-explicit-action-p
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
                            company-preview-frontend
                            company-echo-metadata-frontend
                            ;;kzk/company-doc-buffer-frontend)
                            )

        )

  ;; Company-dabbrev
  (setq company-dabbrev-ignore-invisible t
        ;; company-dabbrev-downcase nil
        ;; company-dabbrev-ignore-case nil
        )


  ;; Company-active shortcuts
  ;; (define-key company-active-map (kbd "C-n") 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  ;; (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "<C-return>") 'company-complete-common)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<C-backspace>") 'company-abort)
  ;; (define-key company-active-map (kbd "<esc>") 'company-abort)
  ;; (define-key company-active-map (kbd "C-w") 'company-abort)
  ;; (define-key company-active-map (kbd "C-e") 'company-other-backend)
  ;; (define-key company-active-map (kbd "C-<f1>") 'kzk/company-show-doc-buffer)
  ;; ;;(define-key company-active-map (kbd ;;"S-TAB" 'company-select-previous)

  (general-define-key :keymaps 'company-mode-map
                      "C-c o" 'company-manual-begin
                      "C-c f" 'company-files
                      "C-c y" 'company-yasnippet)


  (company-quickhelp-mode 1)

  (setq company-quickhelp-delay nil)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company))

;;; Code:

  ;; (general-define-key
  ;;  ;"C-n"  'company-manual-begin
  ;;  "C-c o" 'company-manual-begin
  ;;  "C-c f" 'company-files
  ;;  "C-c k" 'company-keywords
  ;;  "C-c y" 'company-yasnippet
  ;;  :keymaps 'company-mode-map :states '(insert emacs) :prefix nil)

  ;; My take on docbuffers
  (defun kzk/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (company-doc-buffer (company-call-backend 'meta selected)))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))

  ;; A doc-buffer frontend that updates the current docbuffer
  (defun kzk/company-doc-buffer-frontend (command)
    "`company-doc-buffer-frontend' -- shows documentation for current candidate"
    (interactive)
    (when (eq command 'post-command)
      (let* ((selected (nth company-selection company-candidates))
             (meta-text (company-call-backend 'meta selected))
             (doc-buffer (or (company-call-backend 'doc-buffer selected)
                             (company-doc-buffer ""))))
        (if doc-buffer
            (let* ((doc-buffer-win (get-buffer-window doc-buffer)))
              (when doc-buffer-win
                  (with-current-buffer doc-buffer
                    (progn
                      (goto-char (point-min))
                      (insert (concat meta-text "\n\n"))))
                  (display-buffer doc-buffer t)
                  ;(minimize-window doc-buffer-win)
                  (fit-window-to-buffer doc-buffer-win 10 2 nil nil t)))))))

  ;; Predicate to insert completion
  (defun kzk/company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))


  ;; ALWAYS show the tooltip. It's more vim-like and I
  ;; prefer it this way
  ;;(defun company--show-inline-p ()
  ;;  nil)


(provide 'kzk-company)
;;; kzk-company.el ends here
