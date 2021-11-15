;;; packages.el --- kzk-ahs layer packages file for Spacemacs.

;;; Fixes auto-highlight-symbol not disabling highlight on block selection

(defconst kzk-ahs-packages
  '(auto-highlight-symbol
    evil))

(defvar kzk/evil-is-in-visual-mode nil "If evil is currently in visual mode")

(defun kzk-ahs/post-init-auto-highlight-symbol ()
  ;;; Because reasons, push fails without the with-eval-after-load ...
  ;;; probably symbol is not autoloaded
  (with-eval-after-load 'auto-highlight-symbol
    (push 'kzk/evil-is-in-visual-mode ahs-disabled-flags)))

(defun kzk-ahs/post-init-evil ()
  (add-hook 'evil-visual-state-entry-hook
            (lambda ()
              (make-variable-buffer-local 'kzk/evil-is-in-visual-mode)
              (setq kzk/evil-is-in-visual-mode t)))

  (add-hook 'evil-visual-state-exit-hook
            (lambda () (setq kzk/evil-is-in-visual-mode nil))))
