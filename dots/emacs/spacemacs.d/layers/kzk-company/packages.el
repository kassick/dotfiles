;;; packages.el --- kzk-company layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Rodrigo Kassick <kassick@voyager>
;; URL: https://bitbucket.com/kassick/dotfiles
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
    company-quickhelp
    helm-company
    company-posframe))

(defcustom kzk-company/ignored-files-extensions
  '("fbd_latexmk" "aux" "log" "pdf" "bbl"
    "bcf" "gz" "blg" "fls"
    "png" "jpg" "jpeg" "JPEG" "Jpeg")
  "List of extensions for dabbrev to ignore during completion"
  :type '(repeat string)
  :group 'kzk-company)

(defun kzk-company/post-init-company ()
  (setq company-idle-delay 1
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

  ;; TODO: maybe move this to other layer or maybe rename this layer for auto-complete ...
  (with-eval-after-load 'auto-complete
    (setq tab-always-indent t))

  ;; (define-key global-map (kbd "C-<tab>") 'indent-for-tab-command)
  ;; (with-eval-after-load 'company
  ;;   (message "Remapping indent-for-tab-command for company indent-or-complete-common")
  ;;   ;; (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  ;;   (define-key company-mode-map (kbd "<tab>") 'indent-for-tab-command)
  ;;   )

  ;; More vim-like completion
  ;; (require 'company-tng) ;; shipped with company
  ;; (company-tng-configure-default)

  ;; Company-dabbrev
  (with-eval-after-load 'company-dabbrev
    (setq company-dabbrev-ignore-invisible t
          ;; company-dabbrev-downcase nil
          ;; company-dabbrev-ignore-case nil
          ))

  ;; blacklist some buffers
  (kzk/company-blacklist-files)


  ;; Company-active shortcuts
  ;; (define-key company-active-map (kbd "C-n") 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  ;; (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "<C-return>") 'company-complete-common)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<esc>") 'company-abort)
  ;; (define-key company-active-map (kbd "C-e") 'company-other-backend)
  ;; (define-key company-active-map (kbd "C-<f1>") 'kzk/company-show-doc-buffer)
  ;; ;;(define-key company-active-map (kbd ;;"S-TAB" 'company-select-previous)

  (with-eval-after-load 'general
    (with-eval-after-load 'company
      (general-define-key :keymaps 'company-mode-map
                          "C-c o" 'company-manual-begin
                          "C-;" 'company-manual-begin
                          "C-c f" 'company-files
                          "C-c y" 'company-yasnippet)

      (general-define-key :keymaps 'company-active-map
                          "C-;" 'company-complete-common-or-cycle
                          "M-h" #'company-quickhelp-manual-begin
                          "C-e" #'company-other-backend
                          "C-w" 'company-abort
                          "<C-backspace>" 'company-abort
                          )

      (general-define-key :keymaps 'company-active-map
                          :predicate '(company-explicit-action-p)
                          "RET" 'company-complete-selection
                          "<return>" 'company-complete-selection
                          "<home>" 'company-select-first
                          "<end>" 'company-select-last
                          )
      )
    )

    ;;; NO LINGERING COMPANY POPUP AFTER ESCAPING TO NORMAL
    ;;; This is very annoying
    (with-eval-after-load 'evil
      (add-hook 'evil-insert-state-exit-hook 'company-abort))

    ;;; end of kzk-company/post-init-company
    )

(defun kzk-company/post-init-company-quickhelp ()
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
  )

(defun kzk-company/post-init-helm-company ()
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company))
  )

(defun kzk-company/init-company-posframe ()
  ;;; this fixes an issue with a very slow posframe -- something in GNOME interferes with emacs frames causing delays
  ;;; https://github.com/tumashu/company-posframe/issues/2#issuecomment-609945180
  ;;; This is a workaround to have posframe working without 2sec lag under gnome
  ;;; see documentation for possible values. 'hide may introduce flickr, but I haven't noticed it with pgtk builds
  (setq x-gtk-resize-child-frames 'hide)

  (company-posframe-mode))

;;; packages.el ends here
