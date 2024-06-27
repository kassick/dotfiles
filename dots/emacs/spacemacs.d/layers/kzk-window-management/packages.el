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
         popwin:special-display-config)

   ;; Fix popwin/pupo weird behaviours:

   ;; Make help windows not dedicated and make them pop to the right. Let lsp,
   ;; emacs, whatever help use the same popup window.
   ;;
   ;; This improves situations where several popup windows start ocuping the
   ;; bottom of the frame.
   (assoc-delete-all "*Help*" popwin:special-display-config #'string-equal)
   (assoc-delete-all "*lsp-help*" popwin:special-display-config #'string-equal)
   (push '("^\\*\\(.+-\\)?[hH]elp\\*$"
           :dedicated nil :position right :width 0.3 :stick t :noselect t :regexp t)
         popwin:special-display-config)
   (pupo/update-purpose-config) ;; needed, since our config changed ...

   ;; Useful stuff! pop last buffer and switch to some popup
   (spacemacs/set-leader-keys
     "wpl" #'kzk/popup-last-no-select
     "wpL" #'kzk/popup-last
     "wpb" #'kzk/consult-switch-to-popup-buffer
     )

   (advice-add 'pupo/display-function
               :around (lambda (f buf alist)
                         (let ((window (apply f (list buf alist))))
                           (when window
                             ;; push the buffer to the beginning of the list
                             ;; but ensure it is no longer present there
                             (setq kzk/pupo-managed-buffers
                                   (append (list buf)
                                           (kzk/clean-up-pupo-managed-buffers (list buf)))))
                           window)))))

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
    (define-key embark-file-map     (kbd "M-W") (kzk/embark-ace-action find-file))
    (define-key embark-buffer-map   (kbd "M-W") (kzk/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "M-W") (kzk/embark-ace-action bookmark-jump))

    ;; esw
    (define-key embark-file-map     (kbd "M-w") 'kzk/esw-ff)
    (define-key embark-buffer-map   (kbd "M-w") 'kzk/esw-buffer)

    ;; other frame
    (define-key embark-file-map (kbd "C-o") 'find-file-other-frame)
    (define-key embark-buffer-map (kbd "C-o") 'switch-to-buffer-other-frame)

    ;; consult-grep has no embark actions and no map, defining one here
    (defvar-keymap embark-grep-actions-map
      :doc "Keymap for actions for grep actions"
      :parent embark-general-map
      "o" #'kzk/embark-grep-action-other-window
      "C-o" #'kzk/embark-grep-action-other-frame
      "M-w" #'kzk/embark-grep-action-esw
      "M-W" #'kzk/embark-grep-action-ace)

    (add-to-list 'embark-keymap-alist '(consult-grep . embark-grep-actions-map))

    ;; For consult line and consult-line-multi, we must provide a new keymap
    ;; for the consult-location type
    (defvar-keymap embark-location-actions-map
      :doc "Keymap for actions for location"
      :parent embark-general-map
      "o" #'kzk/embark-consult-location-other-window
      "C-o" #'kzk/embark-consult-location-other-frame
      "M-w" #'kzk/embark-consult-location-esw
      "M-W" #'kzk/embark-consult-location-ace)

    (add-to-list 'embark-keymap-alist '(consult-location . embark-location-actions-map))))


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
                     "C-x p" '("Previous Window" . evil-window-mru)

                     ;; resize
                     "C-x C-<left>" 'shrink-window-horizontally
                     "C-x C-<right>" 'enlarge-window-horizontally
                     "C-x C-<down>" 'shrink-window
                     "C-x C-<up>" 'enlarge-window)

 (general-define-key :keymaps 'spacemacs-cmds
                     "w _" 'evil-window-set-height
                     "w |" 'evil-window-set-width)

 ;; help window hacks
 (general-define-key :keymaps 'help-map
                     "D" '("Delete help window" . kzk/delete-help-window)
                     "h" '( "Help for stuff at point" . kzk/evil-smart-doc-lookup))

 (general-define-key :keymaps 'global
                     :prefix dotspacemacs-leader-key
                     :states '(normal motion visual)

                     "hD" '("Delete help window" . kzk/delete-help-window)
                     "cD" '( "Deletes the compilation window" . kzk/delete-compile-window))


 (require 'window)
 (advice-add 'display-buffer-override-next-command :before #'kzk/patch-display-buffer-override-next-command-action-list))
