(defconst kzk-window-management-packages
  `(spacemacs-purpose-popwin
    es-windows
    window-purpose
    posframe
    embark
    ;; (nameframe :location (recipe
    ;;                       :fetcher github
    ;;                       :repo "john2x/nameframe"
    ;;                       :files ("*.el" "dist")))
    ;; ,@(when kzk/experimental-popup-as-side-window
    ;;     '(popper)
    ;;     )
    ))

(defun kzk-window-management/post-init-spacemacs-purpose-popwin ()
  (kzk/after-init
   (require 'spacemacs-purpose-popwin)
   (require 'popwin)
   (require 'window-purpose)
   ;; Customize popwin/pupo

   ;; Make help windows not dedicated and make them pop to the right. Let lsp,
   ;; emacs, whatever help use the same popup window.
   ;;
   ;; This improves situations where several popup windows start ocuping the
   ;; bottom of the frame.
   (assoc-delete-all "*Help*" popwin:special-display-config #'string-equal)
   (assoc-delete-all "*lsp-help*" popwin:special-display-config #'string-equal)
   (push '("^\\*\\(.+-\\)?[hH]elp\\*$"
           :dedicated nil :position right :width 78 :stick t :noselect t :regexp t)
         popwin:special-display-config)

   ;; Info modes are help ---
   (dolist (m '(info-mode Info-mode shortdoc-mode))
     (push `(,m
             :dedicated nil :position right :width 78 :stick t :noselect t)
           popwin:special-display-config))

   ;; Company documentation -- ensure it is shown at bottom
   (push '("*company-documentation*" :height 10 :position bottom :noselect t)
         popwin:special-display-config)

   ;; Embark collect should show at bottom
   (push '(embark-collect-mode :stick t :dedicated t :position bottom :height 0.3 :noselect nil)
         popwin:special-display-config)

   (push '(messages-buffer-mode :dedicated t :position bottom :height 0.4 :stick t :noselect t)
         popwin:special-display-config)

   (push '("*Warnings*" :noselect t :dedicated t :position bottom :height 0.3 :stick nil)
         popwin:special-display-config)

   (push '(comint-mode :noselect t :dedicated nil :position bottom :height 0.3 :stick nil)
         popwin:special-display-config)

   (push '(xref--xref-buffer-mode :dedicated nil :position bottom :height 0.3 :stick nil)
         popwin:special-display-config)

   (push '("*envrc*" :dedicatet t :stick t :position bottom :height 0.3 :noselect t)
         popwin:special-display-config)

   ;; needed, since our config changed ...
   (pupo/update-purpose-config)

   ;; Useful stuff! pop last buffer and switch to some popup
   (spacemacs/set-leader-keys
     "wpo" '("Other popup window" . kzk/other-popup)
     "wpO" `("Other popup window, backwards" . ,(lambda (count) (interactive "p")
                                                  (kzk/other-popup (* -1 count))))
     "wpl" '("Pop last opened popup without selecting the window" . kzk/popup-last-no-select)
     "wpL" '("Pop last opened popup and select window" . kzk/popup-last)
     "wps" '("Select a popup buffer" . kzk/consult-switch-to-popup-buffer)
     "wph" '("Select a popup buffer here" . kzk/consult-switch-to-popup-buffer-same-purpose)
     "wpm" '("Pop messages buffer" . (lambda () (interactive) (display-buffer "*Messages*"))))

   (add-hook 'window-size-change-functions #'kzk/resize-popups-on-frame-change-hook)

   (advice-add 'pupo/display-function
               :around (lambda (f buf alist)
                         (let ((window (apply f (list buf alist))))
                           (when window
                             ;; push the buffer to the beginning of the list
                             ;; but ensure it is no longer present there
                             (purpose-set-window-purpose-dedicated-p window t)
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
  (with-eval-after-load 'window-purpose
    (setcdr (assq 'prefer-other-frame purpose-action-sequences)
            '(purpose-display-maybe-pop-up-frame))

    (setcdr (assq 'prefer-other-window purpose-action-sequences)
            '(purpose-display-reuse-window-buffer
              purpose-display-reuse-window-purpose
              ;; only pops when sensible (`split-window-sensibly')
              ;; in config.el we adjust it to be more permissive
              purpose-display-maybe-pop-up-window
              purpose-display-maybe-other-window
              ;; only pops if `pop-up-frames' says so
              purpose-display-maybe-pop-up-frame
              ;; We DO NOT WANT OTHER (existing) FRAME, we prefer it pops up a
              ;; new one!
              ;;
              ;; --- purpose-display-maybe-other-frame ---
              purpose-display-maybe-same-window))))

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


;; (defun kzk-window-management/init-nameframe ()
;;   (kzk/after-init
;;    (nameframe-projectile-mode t)
;;    (nameframe-eyebrowse-mode t)
;;    (nameframe-perspective-mode t)

;;    (spacemacs/set-leader-keys "F s" '("Switch frame by name" . nameframe-switch-frame ))
;;    (define-key global-map (kbd "C-x 5 s") '("Switch frame by name" . nameframe-switch-frame))))


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
                     "D" '("Delete help window" . kzk/delete-help-window))
 (general-define-key :keymaps 'global
                     "C-S-H" '( "Help for stuff at point" . kzk/evil-smart-doc-lookup) )

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



;; (kzk/after-init
;;  (when kzk/experimental-popup-as-side-window
;;    (setq which-key-side-window-slot -9999)
;;    ;; We could use display-buffer-alist instead of purpose mapped to popb/popr
;;    ;; etc -- not sure purpose is doing anything much interesting
;;    (setq display-buffer-alist nil)
;;    (push '((derived-mode . compilation-mode)
;;            (display-buffer-reuse-window
;;             display-buffer-reuse-mode-window
;;             display-buffer-in-side-window)
;;            (window-height . 0.25)
;;            (side . bottom)
;;            (slot . 0)
;;            (window-parameters . ((no-delete-other-windows . t))))
;;          display-buffer-alist)

;;    (push `((derived-mode . help-mode)
;;            (display-buffer-reuse-window
;;             display-buffer-reuse-mode-window
;;             display-buffer-in-side-window)
;;            (window-width . 0.25)
;;            (side . right)
;;            (slot . 0)
;;            (window-parameters . ((no-delete-other-windows . t))))
;;          display-buffer-alist)

;;    (push '((major-mode . embark-collect-mode)
;;            (display-buffer-reuse-window
;;             display-buffer-reuse-mode-window
;;             display-buffer-in-side-window)
;;            (window-height . 0.25)
;;            (side . bottom)
;;            (slot . -1)
;;            (window-parameters . ((no-delete-other-windows . t))))
;;          display-buffer-alist)
;;    (push '((major-mode . messages-buffer-mode)
;;            (display-buffer-reuse-window
;;             display-buffer-reuse-mode-window
;;             display-buffer-in-side-window)
;;            (window-height . 0.25)
;;            (side . bottom)
;;            (slot . -1)
;;            (window-parameters . ((no-delete-other-windows . t))))
;;          display-buffer-alist)

;;    ;; (push '("^ \\*undo-tree\\*$"
;;    ;;         (display-buffer-reuse-window
;;    ;;          display-buffer-reuse-mode-window
;;    ;;          display-buffer-in-side-window)
;;    ;;         (window-width . 0.25)
;;    ;;         (side . right)
;;    ;;         (slot . -2)
;;    ;;         (window-parameters . ((no-delete-other-windows . t))))
;;    ;;       display-buffer-alist)
;;    ;; (push '(("^\\*undo-tree Diff\\*$")
;;    ;;         (display-buffer-at-bottom)
;;    ;;         (window-width . 0.25)
;;    ;;         (window-height . 10)
;;    ;;         ;; (side . right)
;;    ;;         ;; (slot . -1)
;;    ;;         ;; (select . nil)
;;    ;;         (window-parameters . ((no-delete-other-windows . t))))
;;    ;;       display-buffer-alist)

;;    ;; (with-eval-after-load 'undo-tree
;;    ;;   (advice-add #'undo-tree-visualizer-show-diff
;;    ;;               :around (lambda (fn &rest args)
;;    ;;                         ;; Override split-window! we want it do simply display the buffer
;;    ;;                         (cl-letf (((symbol-function #'split-window)
;;    ;;                                    (lambda (&rest args)
;;    ;;                                      (let ((buff (get-buffer-create undo-tree-diff-buffer-name))
;;    ;;                                            (curr-window (selected-window)))
;;    ;;                                        (display-buffer buff)))))
;;    ;;                           (apply fn args)))))
;;    )
;;  )

;; (defun kzk-window-management/init-popper ()
;;   (message "Popper!!!!!")
;;   (kzk/after-init
;;    (require 'projectile)
;;    (setq popper-display-control nil)
;;    (kzk/sync-popper-popups)
;;    ;; (setq popper-group-function #'popper-group-by-projectile)
;;    ;; (setq popper-reference-buffers
;;    ;;       '("\\*Messages\\*"
;;    ;;         "Output\\*$"
;;    ;;         "\\*Async Shell Command\\*"
;;    ;;         ;; "^\\*lsp-log\\*$"
;;    ;;         ;; "^\\*[a-zA-Z0-9_-]+-lsp\\(::std*\\)?\\*$"
;;    ;;         help-mode
;;    ;;         compilation-mode
;;    ;;         "^\\*ert\\*$"
;;    ;;         quickrun-mode
;;    ;;         dap-server-log-mode
;;    ;;         embark-collect-mode
;;    ;;         grep-mode
;;    ;;         man-mode
;;    ;;         woman-mode
;;    ;;         ;; "^\\*undo-tree Diff\\*$"
;;    ;;         "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
;;    ;;         "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
;;    ;;         "^\\*term.*\\*$"   term-mode   ;term as a popup
;;    ;;         "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
;;    ;;         ;; (lambda (buf) (with-current-buffer buf
;;    ;;         ;;                 (and (derived-mode-p 'fundamental-mode)
;;    ;;         ;;                      (not (string-match-p "^\\*Echo Area .*\\*$") (buffer-name buf))
;;    ;;         ;;                      (< (count-lines (point-min) (point-max))
;;    ;;         ;;                         10))))
;;    ;;         ))
;;    (popper-mode +1)
;;    (popper-echo-mode +1)
;;    ;; (push '("*quickrun*"             :dedicated t :position bottom :stick t :noselect t   :height 0.3) popwin:special-display-config)
;;    ;;   (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
;;    ;;   (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
;;    ;;   (push '(compilation-mode         :dedicated nil :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
;;    ;;   (push '(dap-server-log-mode      :dedicated nil :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
;;    ;;   (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;    ;;   (push '("*undo-tree*"            :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
;;    ;;   (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
;;    ;;   (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;    ;;   (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;    ;;   (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
;;    ;;   (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
;;    ;;   (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
;;    ;;   (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)

;;    (spacemacs/set-leader-keys
;;      "wp <tab>" '("Cycle popups" . popper-cycle)
;;      "wp S-<tab>" '("Cycle popups" . popper-cycle-backwards)
;;      "wpt" '("Toggle last popup" . popper-toggle)
;;      "wpT" '("Toggle all popups" . (lambda () (interactive) (popper-toggle 16)))
;;      "wpl" '("Pop last opened popup without selecting the window" . kzk/popup-last-no-select)
;;      "wpL" '("Pop last opened popup and select window" . kzk/popup-last)
;;      "wps" '("Select a popup buffer" . kzk/consult-switch-to-popup-buffer)
;;      "wph" '("Select a popup buffer here" . kzk/consult-switch-to-popup-buffer-same-purpose)
;;      "wpm" '("Pop messages buffer" . (lambda () (interactive) (display-buffer "*Messages*"))))
;;    ))
