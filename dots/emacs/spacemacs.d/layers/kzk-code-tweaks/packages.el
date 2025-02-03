(defconst kzk-code-tweaks-packages '
  (
   multi-vterm
   projectile
   vterm
   evil-args
   evil-matchit
   evil-easymotion))

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

(defun kzk-code-tweaks/post-init-multi-vterm ())

(defun kzk-code-tweaks/post-init-projectile ()
  (kzk/after-init
   ;; Defer to init -- the shell layer may end up execuring after this one and
   ;; resetting my customization
   (spacemacs/set-leader-keys
     "p'" #'kzk/projectile-pop-shell)))

(defun kzk-code-tweaks/post-init-evil-args ()
  (kzk/after-init
   (define-key evilem-map "L" (evilem-create #'evil-forward-arg))
   (define-key evilem-map "H" (evilem-create #'evil-backward-arg))
   (general-define-key "L" '("Arg Forward" . evil-forward-arg)
                       "H" '("Arg Backward" . evil-backward-arg)
                       "K" '("Jump Out of Args" . evil-jump-out-args)
                       :states '(visual motion normal))))

(defun kzk-code-tweaks/post-init-evil-easymotion ()
  ;; evilem on \
  (evilem-default-keybindings "\\" ))


(defun kzk-code-tweaks/post-init-evil-matchit ()
  (add-hook 'prog-mode-hook (lambda () (evil-matchit-mode t)))
  (add-hook 'text-mode-hook (lambda () (evil-matchit-mode t)))
  )
