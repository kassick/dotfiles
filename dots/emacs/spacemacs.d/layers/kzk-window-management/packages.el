(defconst kzk-window-management-packages
  '(popwin
    es-windows
    window-purpose
    ))

(defun kzk-window-management/post-init-popwin ()
  (kzk/after-init
   (push '("\\*company-documentation\\*" :height 10 :position bottom :noselect t)
         popwin:special-display-config)
   (push '("^\\*Flycheck.+\\*$" :regexp t
           :dedicated t :position bottom :stick t :noselect t)
         popwin:special-display-config)))

(defun kzk-window-management/init-es-windows ()
  (kzk/after-init
   (kzk/setup-helm-esw)
   (general-define-key :keymaps 'global
                       "C-x 7 2" 'esw/select-window
                       "C-x 7 s" 'esw/select-window
                       "C-x 7 m" 'esw/move-window
                       "C-x 7 b" 'esw/show-buffer
                       "C-x 7 C-s" 'esw/swap-two-windows
                       "C-x 7 0" 'esw/delete-window)))

(kzk/after-init
  (general-define-key :keymaps    'global
                      "C-x b"     'helm-mini
                      "C-x C-b"   'helm-mini
                      "<C-f10>"   'ibuffer
                      "<C-S-f10>" 'ibuffer-other-window
                      "C-x <up>"  'windmove-up
                      "C-x <down>" 'windmove-down
                      "C-x <left>" 'windmove-left
                      "C-x <right>" 'windmove-right
                      "C-x p" 'evil-window-mru

                      ;; resize
                      "C-x C-<left>" 'shrink-window-horizontally
                      "C-x C-<right>" 'enlarge-window-horizontally
                      "C-x C-<down>" 'shrink-window
                      "C-x C-<up>" 'enlarge-window
                      )

  (general-define-key :keymaps 'spacemacs-cmds
                      "w _" 'evil-window-set-height
                      "w |" 'evil-window-set-width)

  ;; help window hacks
  (general-define-key :keymaps 'global
                      "C-h D" '(kzk/delete-help-window :which-key "Delete help window"))
  (general-define-key :keymaps 'global
                      :prefix dotspacemacs-leader-key
                      :states '(normal motion visual)

                      "hD" '(kzk/delete-help-window :which-key "Delete help window")
                      "cD" '(kzk/delete-compile-window :which-key "Deletes the compilation window")
                      )
  )

(defun kzk-window-management/post-init-window-purpose ()
  ;; Forcing prefer-other-frame to popup new frame
  (setcdr (assq 'prefer-other-frame purpose-action-sequences)
          '(purpose-display-maybe-pop-up-frame)))
