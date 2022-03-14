;;; packages.el --- kzk-helm layer packages file for Spacemacs.
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
;; added to `kzk-helm-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `kzk-helm/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `kzk-helm/pre-init-PACKAGE' and/or
;;   `kzk-helm/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst kzk-helm-packages
  '(helm
    flyspell-correct-helm
    helm-swoop))

(defun kzk-helm/post-init-helm ()
  ;; Helm genral setq
  ;;; workaround for issue https://github.com/syl20bnr/spacemacs/issues/9549
  (require 'helm-bookmark)

  ;;; Workaround for helm-buffer--details faililng due to missing dired-buffers variable
  (require 'dired)
  (setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-echo-input-in-header-line t
        ;; helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t

        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t

        ;; Helm find files
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-file-name-history-use-recentf t
        helm-ff-fuzzy-matching  t

        ;; Helm-M-x
        helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x

        ;; Helm-mini
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  (kzk/after-init
    (general-define-key :prefix dotspacemacs-leader-key
                        :states '(normal visual motion)
                        "h a"  '(spacemacs/helm-dir-smart-do-search :which-key "Smart Search")
                        "s /" '(helm-find :which-key "Find")
                        ))

  ;; add post-init hooks to setup some hacks on helm
  (eval-after-load 'helm-files #'kzk/helm-ff-hacks-setup)

  ;; Force helm mode
  (helm-mode t))

(defun kzk-helm/post-init-flyspell-correct-helm ()
  ;; Helm flyspell

  (kzk/after-init
    (message "Settings keys for flyspell-mode-map")
    (general-define-key :keymaps 'flyspell-mode-map
                        "C-M-;" 'flyspell-correct-wrapper)

    (general-define-key :keymaps 'flyspell-mode-map
                        :prefix dotspacemacs-leader-key
                        :states '(normal motion visual)
                        "S ;" 'flyspell-correct-wrapper)))

(defun kzk-helm/post-init-helm-swoop ()
  (kzk/after-init
    (general-define-key :keymaps 'global
                        "C-*" 'helm-swoop
                        "C-S-s" 'helm-swoop-without-pre-input)
    (general-define-key :keymaps 'global :states 'motion
                        "C-M-*" 'helm-swoop-from-evil-search)
    (general-define-key :keymaps 'isearch-mode-map
                        "C-*" 'helm-swoop-from-isearch)))


;;; packages.el ends here
