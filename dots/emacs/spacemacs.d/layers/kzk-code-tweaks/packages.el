(defconst kzk-code-tweaks-packages '(vterm))

(add-hook 'prog-mode-hook
          (lambda ()
            ;;; use long lines -- visual lines are annoying for lsp and many
            ;;; programming languages
            (make-variable-buffer-local 'toggle-truncate-lines)
            (setq truncate-lines t)

            ;;; make tab indent -- not complete. surprise auto-completion is
            ;;; annoying
            (setq tab-always-indent t)))

(defun kzk-code-tweaks/post-init-vterm ())
