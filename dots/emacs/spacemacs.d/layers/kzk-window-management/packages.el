(defconst kzk-window-management-packages
  '(popwin
    es-windows
    window-purpose
    posframe
    embark
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
   (general-define-key :keymaps 'global
                       "C-x 7 2" 'esw/select-window
                       "C-x 7 s" 'esw/select-window
                       "C-x 7 m" 'esw/move-window
                       "C-x 7 b" 'esw/show-buffer
                       "C-x 7 C-s" 'esw/swap-two-windows
                       "C-x 7 0" 'esw/delete-window)))


(defun kzk-window-management/post-init-window-purpose ()
  ;; Forcing prefer-other-frame to popup new frame
  (setcdr (assq 'prefer-other-frame purpose-action-sequences)
          '(purpose-display-maybe-pop-up-frame)))

(defun kzk-window-management/post-init-posframe ()
  (advice-add 'delete-frame :around #'kzk/handle-delete-frame-error))

(defun kzk-window-management/post-init-embark ()
  (with-eval-after-load 'embark
    ;; Ace -- easy and done
    (define-key embark-file-map     (kbd "C-c W") (kzk/embark-ace-action find-file))
    (define-key embark-buffer-map   (kbd "C-c W") (kzk/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "C-c W") (kzk/embark-ace-action bookmark-jump))

    ;; esw
    (define-key embark-file-map     (kbd "C-c w") 'kzk/esw-ff)
    (define-key embark-buffer-map   (kbd "C-c w") 'kzk/esw-buffer)

    ;; other frame
    (define-key embark-file-map (kbd "C-o") 'find-file-other-frame)
    (define-key embark-buffer-map (kbd "C-o") 'switch-to-buffer-other-frame)

    ;; consult-grep has no embark actions and no map, defining one here
    (defvar-keymap embark-grep-actions-map
      :doc "Keymap for actions for tab-bar tabs (when mentioned by name)."
      :parent embark-general-map
      "o" #'kzk/embark-grep-action-other-window
      "C-o" #'kzk/embark-grep-action-other-frame
      "C-c w" #'kzk/embark-grep-action-esw
      )

    (add-to-list 'embark-keymap-alist '(consult-grep . embark-grep-actions-map))
    )
  )



;;; Several post-init keybindings
(kzk/after-init
 (when (configuration-layer/layer-used-p 'helm)
   (general-define-key :keymaps    'global
                        "C-x b"     'helm-mini
                        "C-x C-b"   'helm-mini))

   (general-define-key :keymaps    'global
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
  (general-define-key :keymaps 'help-map
                      "D" '(kzk/delete-help-window :which-key "Delete help window"))
  (general-define-key :keymaps 'global
                      :prefix dotspacemacs-leader-key
                      :states '(normal motion visual)

                      "hD" '(kzk/delete-help-window :which-key "Delete help window")
                      "cD" '(kzk/delete-compile-window :which-key "Deletes the compilation window")
                      )
  )
