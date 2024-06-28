(defconst kzk-window-management-packages
  '(popwin
    es-windows
    window-purpose
    posframe
    embark
    ))

(defun kzk-window-management/post-init-popwin ()
  (kzk/after-init
   ;; Customize popwin/pupo

   ;; Company documentation -- ensure it is shown at bottom
   (push '("\\*company-documentation\\*" :height 10 :position bottom :noselect t)
         popwin:special-display-config)

   ;; Embark collect should show at bottom
   (push '(embark-collect-mode :dedicated nil :position bottom :height 0.3 :noselect nil)
         popwin:special-display-config)

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


   (push '(messages-buffer-mode :dedicated nil :position bottom :height 0.4 :stick nil)
         popwin:special-display-config)

   ;; needed, since our config changed ...
   (pupo/update-purpose-config)

   ;; Useful stuff! pop last buffer and switch to some popup
   (spacemacs/set-leader-keys
     "wpl" '("Pop last opened popup without selecting the window" . kzk/popup-last-no-select)
     "wpL" '("Pop last opened popup and select window" . kzk/popup-last)
     "wps" '("Select a popup buffer" . kzk/consult-switch-to-popup-buffer)
     "wph" '("Select a popup buffer here" . kzk/consult-buffer-with-purpose)
     "wpm" '("Pop messages buffer" . (lambda () (interactive) (display-buffer "*Messages*"))))

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

 ;; Scroll last active window / esw select target window
 (setq other-window-scroll-default #'kzk/other-window-default-cb)
 (add-hook 'window-configuration-change-hook #'kzk/maybe-reset-other-window-parameter)

 (general-define-key :keymaps 'evil-window-map
                     "C-o" '("Set other window for this window" . kzk/esw-set-other-window)
                     "C-M-o" '("Reset other window". kzk/reset-other-window))
 (spacemacs/set-leader-keys
   "w C-o" '("Set other window for this window" . kzk/esw-set-other-window)
   "w C-M-o" '("Reset other window" . kzk/reset-other-window))

 ;; maybe helm?
 (when (configuration-layer/layer-used-p 'helm)
   (general-define-key :keymaps    'global
                       "C-x b"     'helm-mini
                       "C-x C-b"   'helm-mini))

 ;; Some basic window movements
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

 ;; Minibuffer interaction hacks
 (spacemacs/set-leader-keys
   "wB" '("Quits the active minibuffer". kzk/quit-active-minibuffer))


 (require 'window)
 (advice-add 'display-buffer-override-next-command :before #'kzk/patch-display-buffer-override-next-command-action-list))
