(defconst kzk-copilot-packages
  '(;; (copilot :location (recipe
    ;;                     :fetcher github
    ;;                     :repo "copilot-emacs/copilot.el"
    ;;                     :files ("*.el" "dist")))
    lsp-mode
    shell-maker
    aider
    (copilot-chat :location (recipe
                             :fetcher github
                             :repo "chep/copilot-chat.el"
                             :files ("*.el")))))

(defun kzk-copilot/init-shell-maker ()
  (use-package shell-maker
    :defer t))

(defun kzk-copilot/init-aider ()
  (use-package aider
    :defer t
    :init
    ;; missing autoload
    (autoload 'aider-transient-menu-1col "aider" "Transient menu for Aider commands." t)

    :config
    (evil-define-key '(insert) aider-comint-mode-map
      (kbd "C-r") #'comint-history-isearch-backward-regexp))

  (kzk/after-init
   (define-key kzk/copilot-chat-map (kbd "%")
               '("Aider" . aider-transient-menu-1col))

   (define-key global-map (kbd "C-%") '("Aider Transient Menu" . aider-transient-menu-1col))
   (spacemacs/set-leader-keys "%" '("Aider Transient Menu" . aider-transient-menu-1col))

   (with-eval-after-load 'embark
     (define-key embark-buffer-map
                 (kbd "M-%") #'kzk/aider-add-buffer))

   (push '("^\\*aider:.*\\*$" :regexp t :width 78 :position left :stick t :noselect nil :dedicated nil)
         popwin:special-display-config)))


(defun kzk-copilot/init-copilot-chat ()
  (use-package copilot-chat
    :defer t
    :init

    ;; :init (message "copilot chat init")
    ;; :custom (copilot-chat-frontend 'shell-maker)
    :config
    ;;(message "copilot chat config")

    ;; Prompt window:
    ;; - Some helper keys for normal
    (evil-define-key '(normal motion) copilot-chat-org-prompt-mode-map
      ;; "q" 'kzk/copilot-chat-delete-windows
      "L" 'copilot-chat-prompt-split-and-list
      "J" 'copilot-chat-prompt-history-next
      "K" 'copilot-chat-prompt-history-previous)

    ;; - Send on c-return -- make it easier to write multi-line prompts
    (define-key copilot-chat-org-prompt-mode-map
                (kbd "C-<return>") 'copilot-chat-prompt-send)

    ;; chat buffer list window:
    ;; - Evilify state
    (evilified-state-evilify-map copilot-chat-list-mode-map
      :mode copilot-chat-list-mode
      :bindings
      (kbd "<ret>") 'copilot-chat-list-add-or-remove-buffer
      (kbd "C-SPC") 'copilot-chat-list-add-or-remove-buffer
      (kbd "g") 'copilot-chat-list-refresh
      (kbd "C") 'copilot-chat-list-clear-buffers)


    ;; ;; dont use pupo, because the way that copilot chat displays the buffer is
    ;; ;; kind of weird
    ;; (purpose-set-extension-configuration
    ;;   :kzk-copilot-layer
    ;;   (purpose-conf :regexp-purposes `((,kzk/copilot-chat-buffer-regexp . coding-assistant)
    ;;                                    ;;("^\\*[Cc]opilot-chat-prompt\\*$" . coding-assistant-prompt)
    ;;                                    (,kzk/copilot-chat-buffer-list-regexp . coding-assistant-others))))

    ;; ;; reuse whenever
    ;; (dolist (it '(coding-assistant coding-assistant-others))
    ;;   (push `(coding-assistant-prompt
    ;;           purpose-display-reuse-window-buffer
    ;;           purpose-display-reuse-window-purpose)
    ;;         purpose-special-action-sequences))

    ;; (push `(coding-assistant
    ;;         purpose-display-reuse-window-buffer
    ;;         purpose-display-reuse-window-purpose
    ;;         kzk/copilot-chat-popup-display
    ;;         ;; ,(pupo//position-to-display-function 'left 78 nil)
    ;;         ;; purpose-display-pop-up-window
    ;;         )
    ;;       purpose-special-action-sequences)

    (add-hook 'copilot-chat-org-prompt-mode-hook #'kzk/copilot-chat-mode-hook)
    (push `(,kzk/copilot-chat-buffer-regexp :regexp t :width 78 :position left :stick t :noselect nil :dedicated nil)
          popwin:special-display-config)
    (pupo/update-purpose-config)

    ;; TODO: Fix windowe placement!!! because polymode calls switch-to-buffer
    ;; in pm--select-existing-buffer-visibly , it ends up running though the
    ;; purpose code again. For reasons, it ends up popping up in a different
    ;; window, resulting in a very inconsistent state...

    ;; end of use package
    )

  (kzk/after-init

   ;; (advice-add 'copilot-chat-display :before #'copilot-chat-hide)
   (advice-add 'copilot-chat-hide :override 'kzk/copilot-chat-hide)
   ;; (advice-add 'copilot-chat-reset :override 'kzk/copilot-chat-reset)

   ;; (setq kzk/copilot-chat-map (make-sparse-keymap))

   (dolist (fn '(copilot-chat-explain
                 copilot-chat-review
                 copilot-chat-doc
                 copilot-chat-fix
                 copilot-chat-optimize
                 copilot-chat-test))
     (advice-add fn :after (lambda (&rest _) (copilot-chat-display))))

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
                     ;; Hide from outside is not working
                     ;; ("0" . ("Delete Copilot Chat Windows" . copilot-chat-hide))
                     ("." . ("Chat Transient Mode" . copilot-chat-transient))
                     ("C-." . ("Chat Transient Code" . copilot-chat-transient-code))
                     ("C-b" . ("Chat Transient Buffers" . copilot-chat-transient-buffers))
                     ("C-c" . (menu-item "Chat Transient Magit"
                                         copilot-chat-transient-magit
                                         :filter ,(lambda (item)
                                                    (if (bound-and-true-p git-commit-mode)
                                                        `("Chat Transient Magit" . ,item)
                                                      '("Chat Transient Magit (disabled)" . ignore)
                                                      ;;#'ignore
                                                      )))))))
     (dolist (binding bindings)
       (define-key kzk/copilot-chat-map (kbd (car binding)) (cdr binding)))))

  (with-eval-after-load 'embark
    (define-key embark-buffer-map
                (kbd "M-&") #'kzk/copilot-chat-add-buffer)))

;; -------

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
