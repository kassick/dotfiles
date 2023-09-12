(defconst kzk-go-packages '(go-mode lsp-mode))

(defun kzk-go/post-init-go-mode ()
  ;;(spacemacs/set-leader-keys-for-major-mode 'go-mode "==" 'lsp-format-buffer)
  nil)

  ;; (message "not setting gofmt key")
  ;; (with-eval-after-load 'lsp-mode
  ;;   (define-key spacemacs-lsp-mode-map (kbd "==") 'lsp-format-buffer)))

(defun kzk-go/post-init-lsp-mode ()
  (custom-set-variables
   '(lsp-go-hover-kind "FullDocumentation")))
