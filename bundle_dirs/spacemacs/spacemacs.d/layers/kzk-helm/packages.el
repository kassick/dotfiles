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
    helm-flyspell
    helm-projectile
    helm-swoop
    ;; general
    )
  "The list of Lisp packages required by the kzk-helm layer.

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

(defun kzk-helm/post-init-helm ()
  ;; Helm genral setq
  (setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-echo-input-in-header-line t
                                        ;helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)

  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)

  ;; Helm find files
  (setq helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-file-name-history-use-recentf t
        hhelm-ff-fuzzy-matching  nil)

  ;; Helm-M-x
  (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

  ;; Helm-mini
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  (require 'general)
  (general-define-key :prefix dotspacemacs-leader-key :states '(normal visual motion)
                      "h a"  '(helm-ag :which-key "Ag (cwd)")
                      "s /" '(helm-find :which-key "Find")
                      )

  ;; add post-init hooks to setup some hacks on helm
  (eval-after-load 'helm-files #'kzk/helm-ff-hacks-setup)
  (eval-after-load 'helm-buffers #'kzk/helm-buffers-hacks-setup)
  )

(defun kzk-helm/post-init-helm-projectile ()
  (eval-after-load 'helm-projectile #'kzk/helm-projectile-hacks-setup)
  )

(defun kzk-helm/post-init-helm-flyspell ()
  ;; Helm flyspell
  (require 'general)
  (general-define-key :keymaps 'flyspell-mode-map
                      "C-;" 'helm-flyspell-correct)

  (general-define-key :keymaps 'flyspell-mode-map
                      :prefix dotspacemacs-leader-key
                      :states '(normal motion visual)
                      "S ;" 'helm-flyspell-correct)
)

(defun kzk-helm/post-init-helm-swoop ()
  (general-define-key :keymaps 'global
                      "C-*" 'helm-swoop
                      "C-S-s" 'helm-swoop-without-pre-input)
  (general-define-key :keymaps 'global :states 'motion
                      "C-M-*" 'helm-swoop-from-evil-search)
  (general-define-key :keymaps 'isearch-mode-map
                      "C-*" 'helm-swoop-from-isearch
                      )
  )

(defun kzk-helm/post-init-general ()
  ; nop
  )

;;; packages.el ends here
