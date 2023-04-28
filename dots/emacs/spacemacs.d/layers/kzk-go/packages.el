(defconst kzk-go-packages '(go-mode))

(defun kzk-go/post-init-go-mode ()
  (message "not setting gofmt key")
  (with-eval-after-load 'general
    (general-define-key :keymaps 'go-mode-map
                        :states '(normal motion visual)
                        :prefix dotspacemacs-major-mode-leader-key
                        "ZF" '(gofmt :which-key "Go FMT")))
  ;; (spacemacs/set-leader-keys-for-major-mode 'go-mode
  ;;   "=G" 'gofmt)
  ;; (describe-map 'spacemacs-go-mode-map-prefix)
  )
