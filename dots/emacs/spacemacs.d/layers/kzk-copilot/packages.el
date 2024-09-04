(defconst kzk-copilot-packages
  '((copilot :location (recipe
                        :fetcher github
                        :repo "copilot-emacs/copilot.el"
                        :files ("*.el" "dist")))
    shell-maker
    (copilot-chat :location (recipe
                             :fetcher github
                             :repo "chep/copilot-chat.el"
                             :files ("*.el")))))

(defun kzk-copilot/init-copilot ()
  (use-package copilot
    :defer t
    :hook ((prog-mode . copilot-mode))
    :config
     (message "Copilot init")
     (define-key copilot-mode-map
                 (kbd "C-*") '("Complete with Copilot" . copilot-complete))
     (define-key copilot-mode-map
                 (kbd "C-S-*") '("Panel Completions with Copilot" . copilot-panel-complete))

     (define-key copilot-completion-map (kbd "<return>") 'copilot-accept-completion)
     (define-key copilot-completion-map (kbd "C-l") 'copilot-accept-completion)
     (define-key copilot-completion-map (kbd "<tab>") 'copilot-next-completion)
     (define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion)
     (define-key copilot-completion-map (kbd "C-j") 'copilot-next-completion)
     (define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion)
     (define-key copilot-completion-map (kbd "C-k") 'copilot-previous-completion)
     (define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion-by-word)
     (define-key copilot-completion-map (kbd "C-S-l") 'copilot-accept-completion-by-word)))


(defun kzk-copilot/init-shell-maker ()
  (use-package shell-maker
    :defer t
    :init
    (message "shell-maker init")
    :config
    (message "shell-maker config")
    )
  )

(defun kzk-copilot/init-copilot-chat ()
  (use-package copilot-chat
    :defer t
    :init
    (message "copilot chat init")
    ;; :custom (copilot-chat-frontend 'shell-maker)
    :config
    (message "copilot chat config")

    ;; Prompt window:
    (evil-define-key 'normal copilot-chat-prompt-mode-map
      (kbd "k") 'copilot-chat-prompt-split-and-list
      (kbd "j") 'copilot-chat-prompt-history-next
      (kbd "k") 'copilot-chat-prompt-history-previous
      (kbd "q") 'kzk/copilot-chat-delete-windows)
    (define-key copilot-chat-prompt-mode-map
                (kbd "C-c q") 'kzk/copilot-chat-delete-windows)
    (evil-set-initial-state 'copilot-chat-prompt-mode 'insert)

    ;; chat buffer list window:
    (evilified-state-evilify-map copilot-chat-list-mode-map
      :mode copilot-chat-list-mode
      :bindings
      (kbd "<ret>") 'copilot-chat-list-add-or-remove-buffer
      (kbd "C-SPC") 'copilot-chat-list-add-or-remove-buffer
      (kbd "g") 'copilot-chat-list-refresh
      (kbd "C") 'copilot-chat-list-clear-buffers
      (kbd "q") 'delete-window)

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

    ;; Shell-maker as frontend -- I've found some issues, as I can't pin the
    ;; shell window to the side -- it appears that, upon scrolling, polymode
    ;; is kicking in and displaying the buffer in a window other than the one
    ;; shell-maker is currently opened. I've tried displaying it in another
    ;; frame, but it only creates the frame if the shell buffer does not
    ;; exist, so I'd have to work around it ....

    ;; (require 'shell-maker)
    ;; (require 'copilot-chat-shell-maker) (push '(shell-maker .
    ;; copilot-chat-shell-maker-init) copilot-chat-frontend-list)
    ;; (copilot-chat-shell-maker-init) (setq shell-maker-display-function
    ;; #'kzk/shell-maker-display-buffer)
    )


  (kzk/after-init

   (advice-add 'copilot-chat-display :before #'kzk/copilot-chat-delete-windows)

   (setq kzk-copilot-chat-map (make-sparse-keymap))
   (define-key kzk-copilot-chat-map
               (kbd "c") '("Copilot Chat" . copilot-chat-display))
   (define-key kzk-copilot-chat-map
               (kbd "a") '("Copilot Ask and Insert" . copilot-chat-ask-and-insert))
   (define-key kzk-copilot-chat-map
               (kbd "+") `("Copilot Add Current Buffer" . ,(lambda ()
                                                                 (interactive)
                                                                 (require 'copilot-chat)
                                                                 (copilot-chat-add-current-buffer))))

   ;; These are region only, should probably migrate to general or evil define key
   (define-key kzk-copilot-chat-map
               (kbd "e") '("Copilot Explain" . copilot-chat-explain))
   (define-key kzk-copilot-chat-map
               (kbd "r") '("Copilot Review" . copilot-chat-review))
   (define-key kzk-copilot-chat-map
               (kbd "h") '("Copilot Write Documentation" . copilot-chat-doc))
   (define-key kzk-copilot-chat-map
               (kbd "f") '("Copilot Fix" . copilot-chat-fix))
   (define-key kzk-copilot-chat-map
               (kbd "o") '("Copilot Optimize" . copilot-chat-optimize))
   (define-key kzk-copilot-chat-map
               (kbd "t") '("Copilot Tests" . copilot-chat-test))
   (define-key kzk-copilot-chat-map
               (kbd "k") '("Delete Copilot Chat Windows" . kzk/copilot-chat-delete-windows))

   (define-key global-map (kbd "C-&") kzk-copilot-chat-map)
   (spacemacs/set-leader-keys "&" `("Copilot Chat" . ,kzk-copilot-chat-map))
   (spacemacs/set-leader-keys "a c p" `("Copilot Chat" . ,kzk-copilot-chat-map))

   ;; Chat window placement
   ;; (push '("^\\*copilot-chat-shell\\*" :regexp t :dedicated nil :position right :width 78 :stick nil :noselect nil)
   ;;        popwin:special-display-config)
   (push '(copilot-chat-prompt-mode :position right :width 78 :dedicated nil :stick nil) popwin:special-display-config)
   (push '(copilot-chat-mode :position right :width 78 :dedicated nil :stick nil) popwin:special-display-config)
   (push '(copilot-chat-list-mode :position right :width 20 :dedicated nil :stick nil) popwin:special-display-config)
   (pupo/update-purpose-config)
   )

  (with-eval-after-load 'embark
    (define-key embark-buffer-map
                "&" #'kzk/copilot-chat-add-buffer)
    )

  )
