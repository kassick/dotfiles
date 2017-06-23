;;; packages.el --- kzk-company layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Rodrigo Kassick <kassick@voyager>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `kzk-company-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `kzk-company/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `kzk-company/pre-init-PACKAGE' and/or
;;   `kzk-company/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst kzk-company-packages
  '(company
    general
    company-quickhelp
    helm-company)
  "The list of Lisp packages required by the kzk-company layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun kzk-company/post-init-company ()
  (global-company-mode 1)
  (setq company-idle-delay 0.2
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


  )

(defun kzk-company/post-init-company-quickhelp ()
  (company-quickhelp-mode 1)

  (setq company-quickhelp-delay nil)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
  )

(defun kzk-company/post-init-helm-company ()
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company))
  )

;;; packages.el ends here