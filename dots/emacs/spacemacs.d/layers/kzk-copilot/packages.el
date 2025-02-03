(defconst kzk-copilot-packages
  '(;; (copilot :location (recipe
    ;;                     :fetcher github
    ;;                     :repo "copilot-emacs/copilot.el"
    ;;                     :files ("*.el" "dist")))
    lsp-mode
    shell-maker
    (copilot-chat :location (recipe
                             :fetcher github
                             :repo "chep/copilot-chat.el"
                             :files ("*.el")))))

(defun kzk-copilot/init-shell-maker ()
  (use-package shell-maker
    :defer t))

(defun kzk-copilot/init-copilot-chat ()
  (use-package copilot-chat
    :defer t
    ;; :init (message "copilot chat init")
    ;; :custom (copilot-chat-frontend 'shell-maker)
    :config
    ;;(message "copilot chat config")

    ;; Prompt window:
    (evil-define-key '(normal motion) copilot-chat-prompt-mode-map
      (kbd "k") 'copilot-chat-prompt-split-and-list
      (kbd "j") 'copilot-chat-prompt-history-next
      (kbd "k") 'copilot-chat-prompt-history-previous
      (kbd "q") 'kzk/copilot-chat-delete-windows)
    (define-key copilot-chat-prompt-mode-map
                (kbd "C-c q") 'kzk/copilot-chat-delete-windows)
    (define-key copilot-chat-prompt-mode-map
                (kbd "<return>") 'copilot-chat-prompt-send)
    (evil-set-initial-state 'copilot-chat-prompt-mode 'insert)

    ;; chat buffer list window:
    (evilified-state-evilify-map copilot-chat-list-mode-map
      :mode copilot-chat-list-mode
      :bindings
      (kbd "<ret>") 'copilot-chat-list-add-or-remove-buffer
      (kbd "C-SPC") 'copilot-chat-list-add-or-remove-buffer
      (kbd "g") 'copilot-chat-list-refresh
      (kbd "C") 'copilot-chat-list-clear-buffers)

    ;; Chat Results map
    ;; - Switch to prompt on usual change keys
    (dolist (k '("&" "o" "O" "a" "A" "i" "I" "C"))
      (evil-define-key '(normal motion) copilot-chat-mode-map
        (kbd k) 'kzk/copilot-pop-to-chat-buffer))
    ;; - Quitting deletes the chat windows
    (evil-define-key '(normal motion) copilot-chat-mode-map
      (kbd "q") 'kzk/copilot-chat-delete-windows)
    (define-key copilot-chat-mode-map
                (kbd "C-c q") 'kzk/copilot-chat-delete-windows)

    ;; Shell-maker as frontend -- I've found some issues...
    ;;
    ;; - By using purpose and special-action-sequences, I've fixed the positioning issue
    ;;
    ;; - Current Issue: Shell-maker calls copilot-chat-reset in
    ;;   copilot-chat-shell-maker-display when not ready-p. But after closing
    ;;   the shell-maker window once, for some reason, the instance is no
    ;;   longer ready and reset is called. But shellmaker overrides
    ;;   copilot-chat-reset to a function that resets the advices ... this is
    ;;   probably wrong, but I do not have time to fix it now. Must figure out
    ;;   a) why is it becoming non-ready? and b) is resetting the advices expected?


    (require 'shell-maker)
    (require 'copilot-chat-shell-maker)
    (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
    (copilot-chat-shell-maker-init)
    ;; Removing the advice sees to work!
    (advice-remove 'copilot-chat--clean #'copilot-chat--shell-maker-clean)

    (purpose-set-extension-configuration
     :kzk-copilot-layer
     (purpose-conf :regexp-purposes '(("^\\*[Cc]opilot-chat\\*$" . coding-assistant)
                                      ("^\\*[Cc]opilot-chat-list\\*$" . coding-assistant))))

    (push `(coding-assistant
            purpose-display-reuse-window-buffer
            purpose-display-reuse-window-purpose
            ,(pupo//position-to-display-function 'left 78 nil)
            purpose-display-pop-up-window)
          purpose-special-action-sequences)

    ;; (push '(copilot-chat-shell-mode :position left :width 78 :dedicated t :stick nil) popwin:special-display-config)
    ;; (push '(copilot-chat-list-mode :position left :width 20 :dedicated t :stick nil) popwin:special-display-config)


    (evil-define-key '(normal motion) copilot-chat-shell-mode-map
      "q" #'evil-quit
      "L" #'copilot-chat-prompt-split-and-list)

    (evil-define-key '(insert) copilot-chat-shell-mode-map
      (kbd "C-c q") #'evil-quit
      (kbd "C-c l") #'copilot-chat-prompt-split-and-list)

    ;; end of use package
    )

  (kzk/after-init

   (advice-add 'copilot-chat-display :before #'kzk/copilot-chat-delete-windows)

   ;; For some reason, the custom shortcuts defines in :config do not take
   ;; effect until switching states. This advice, besides killing the chat
   ;; windows before displaying (window management workarounds ugh...) also
   ;; switches states to make sure that the shortcuts are available
   ;; (advice-add 'copilot-chat-shell-maker-display :around (lambda (fn &rest args)
   ;;                                                         (kzk/copilot-chat-delete-windows)
   ;;                                                         (-when-let* ((buf (apply fn args))
   ;;                                                                      (w (get-buffer-window buf)))
   ;;                                                           (with-selected-window w
   ;;                                                             (evil-normal-state)
   ;;                                                             (evil-insert-state)
   ;;                                                             (goto-char (point-max))))))
   (setq kzk-copilot-chat-map (make-sparse-keymap))

   (let ((bindings `(("c" . ("Copilot Chat (current buffer in the context)" . kzk/copilot-chat-display))
                     ("C" . ("Copilot Chat" . copilot-chat-display))
                     ("a" . ("Copilot Ask and Insert" . copilot-chat-ask-and-insert))
                     ("+" . ("Copilot Add Current Buffer" . kzk/copilot-add-current-buffer))
                     ("p" . ("Project" . ,(kzk//make-copilot-context-map "Project" 'kzk/copilot-add-projectile-buffers) ))
                     ("l" . ("Layout" . ,(kzk//make-copilot-context-map "Layout" 'kzk/copilot-add-layout-buffers)))
                     ("F" . ("Frame" . ,(kzk//make-copilot-context-map "Frame" 'kzk/copilot-add-frame-buffers)))
                     ("e" . ("Copilot Explain" . copilot-chat-explain))
                     ("r" . ("Copilot Review" . copilot-chat-review))
                     ("h" . ("Copilot Write Documentation" . copilot-chat-doc))
                     ("f" . ("Copilot Fix" . copilot-chat-fix))
                     ("o" . ("Copilot Optimize" . copilot-chat-optimize))
                     ("t" . ("Copilot Tests" . copilot-chat-test))
                     ("0" . ("Delete Copilot Chat Windows" . kzk/copilot-chat-delete-windows)))))
     (dolist (binding bindings)
       (define-key kzk-copilot-chat-map (kbd (car binding)) (cdr binding))))

   (define-key global-map (kbd "C-&") kzk-copilot-chat-map)
   (spacemacs/set-leader-keys "&" `("Copilot Chat" . ,kzk-copilot-chat-map))
   (spacemacs/set-leader-keys "a c p" `("Copilot Chat" . ,kzk-copilot-chat-map))

   ;; ;; Window PLacement: Try with purpose configuration
   ;; (purpose-set-extension-configuration
   ;;  :kzk-copilot-layer
   ;;  (purpose-conf :name-purposes '(("*Copilot-chat*" . coding-assistant-results)
   ;;                                 ("*Copilot-chat-prompt*" . coding-assistant-prompt))))


   ;; ;; Place the result window at right
   ;; (push `(coding-assistant-results
   ;;         purpose-display-reuse-window-buffer
   ;;         purpose-display-reuse-window-purpose
   ;;         ,(pupo//position-to-display-function 'left 78 nil))
   ;;       purpose-special-action-sequences)

   ;; (push `(coding-assistant-prompt
   ;;         purpose-display-reuse-window-buffer
   ;;         purpose-display-reuse-window-purpose)
   ;;       purpose-special-action-sequences)

   ;; ;; Place the prompt window below
   ;; (push `(coding-assistant-results
   ;;         purpose-display-reuse-window-buffer
   ;;         purpose-display-reuse-window-purpose
   ;;         ,(pupo//position-to-display-function 'right 78 nil))
   ;;       purpose-special-action-sequences)
   ;; Chat window placement
   ;; (push '("^\\*copilot-chat-shell\\*" :regexp t :dedicated nil :position right :width 78 :stick nil :noselect nil)
   ;;        popwin:special-display-config)
   ;; (push '(copilot-chat-prompt-mode :position left :width 78 :dedicated nil :stick t) popwin:special-display-config)
   ;; (push '(copilot-chat-mode :position left :width 78 :dedicated nil :stick t) popwin:special-display-config)
   ;; (push '(copilot-chat-list-mode :position left :width 20 :dedicated nil :stick nil) popwin:special-display-config)
   (pupo/update-purpose-config)
   )

  (with-eval-after-load 'embark
    (define-key embark-buffer-map
                "&" #'kzk/copilot-chat-add-buffer)))

(defun kzk-copilot/post-init-lsp-mode ()
  (with-eval-after-load 'evil-evilified-state
    (evilified-state-evilify-map lsp-copilot-panel-buffer-mode-map :mode lsp-copilot-panel-buffer-mode :eval-after-load lsp-copilot))

  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map
                (kbd "M-*") '("Copilot Panel" . lsp-copilot-panel-completion)))

  (kzk/after-init
   (require 'spacemacs-purpose-popwin)
   (require 'popwin)
   (require 'window-purpose)

   (assoc-delete-all 'lsp-copilot-panel-buffer-mode popwin:special-display-config)
   (push '(lsp-copilot-panel-buffer-mode :noselect nil :dedicated nil :position right :width 0.5 :stick t)
         popwin:special-display-config)

   ;; needed, since our config changed ...
   (pupo/update-purpose-config)



   )

  )
