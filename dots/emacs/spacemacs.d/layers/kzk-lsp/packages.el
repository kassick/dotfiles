(defconst kzk-lsp-packages
  '(general
    lsp-mode
    lsp-ui))


(defun kzk-lsp/post-init-general ())


(defun kzk-lsp/post-init-lsp-mode ()
  (general-define-key :keymaps 'lsp-mode-map
                      "C-c C-h" '("Doc Glance" . lsp-ui-doc-glance)
                      "C-c h" '("Help (no focus)" . kzk/lsp-help-at-point))

  ;; (message "setting lsp-mode minor mode == from kzk-lsp")
  ;; (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
  ;;   "==" #'lsp-format-buffer)
  ;; (define-key spacemacs-lsp-mode-map (kbd "==") 'lsp-format-buffer)
  ;;(spacemacs/set-leader-keys-for-minor-mode 'lsp-mode "==" 'lsp-format-buffer)
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp-warn :around #'kzk/lsp-warn-advice)
    (advice-add 'lsp-workspace-command-execute :override #'kzk/lsp-workspace-command-execute)
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]worktrees\\'" t)

    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "FC" 'kzk/lsp-workspaces-gc)

    (add-hook 'lsp-before-inline-completion-hook #'kzk/save-inline-completion-tick))

  ;; Inline Completions
  (with-eval-after-load 'lsp-inline-completion

    (add-hook 'lsp-inline-completion-accepted-functions
              (lambda (test start end)
                (message "start is %S end is %S" start end )
                (pulse-momentary-highlight-region start end)))

    ;; Pop to Panel Completions
    ;; (WIP on my part, requires lsp-mode from my copy)
    (define-key lsp-inline-completion-active-map
                (kbd "M-*")
                #'kzk/lsp-inline-completion-pop-to-panel))

  (with-eval-after-load 'lsp-copilot
    (evilified-state-evilify-map lsp-copilot-panel-buffer-mode-map :mode lsp-copilot-panel-buffer-mode :eval-after-load lsp-copilot))

  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map
                (kbd "M-*") '("Copilot Panel" . lsp-copilot-panel-completion)))

  (kzk/after-init
   ;; Update popwin config to place the panel at the right place
   (require 'spacemacs-purpose-popwin)
   (require 'popwin)
   (require 'window-purpose)

   (assoc-delete-all 'lsp-copilot-panel-buffer-mode popwin:special-display-config)
   (push '(lsp-copilot-panel-buffer-mode :noselect nil :dedicated nil :position right :width 0.5 :stick t)
         popwin:special-display-config)

   ;; needed, since our config changed ...
   (pupo/update-purpose-config))

  ;; (advice-add 'lsp--client-capabilities :around #'kzk/lsp--client-capabilities-advice)
  ;; (with-eval-after-load 'lsp-ruff
  ;;   ;; Override ruff server definition, providing custom capabilities that
  ;;   ;; will be merged with the default ones
  ;;   (lsp-register-client
  ;;    (make-lsp-client
  ;;     :new-connection (lsp-stdio-connection
  ;;                      (lambda () (append lsp-ruff-server-command lsp-ruff-ruff-args)))
  ;;     :activation-fn (lsp-activate-on "python")
  ;;     :server-id 'ruff
  ;;     :priority -2
  ;;     :add-on? t
  ;;     :custom-capabilities '((general . ((positionEncodings . ["utf-16"]))))
  ;;     :initialization-options
  ;;     (lambda ()
  ;;       (list :settings
  ;;             (list :logLevel lsp-ruff-log-level
  ;;                   :showNotifications lsp-ruff-show-notifications
  ;;                   :organizeImports (lsp-json-bool lsp-ruff-advertize-organize-imports)
  ;;                   :fixAll (lsp-json-bool lsp-ruff-advertize-fix-all)
  ;;                   :importStrategy lsp-ruff-import-strategy))))))
  )

(defun kzk-lsp/post-init-lsp-ui ()
  (advice-add #'lsp-ui-peek--peek-new :around #'kzk/lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :around #'kzk/lsp-ui-peek--peek-destroy)

  (with-eval-after-load 'lsp-mode
    (define-key lsp-command-map (kbd "T i") #'kzk/lsp-toggle-inlay-hints))

  (unless lsp-use-upstream-bindings
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "Tli" #'kzk/lsp-toggle-inlay-hints)))
