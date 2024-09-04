(defconst kzk-diminish-packages '(diminish))

(defun kzk-diminish/post-init-diminish ()
  (when (eq (spacemacs/get-mode-line-theme-name) 'vanilla)
    (add-hook 'after-init-hook #'spacemacs/diminish-hook))

  (kzk/after-init
   (require 'diminish)
   (with-eval-after-load 'spacemacs-whitespace-cleanup
     (diminish 'whitespace-cleanup-mode "Ws"))

   (with-eval-after-load 'ts-fold
     (diminish 'ts-fold-mode ""))

   (with-eval-after-load 'tree-sitter-hl
     (diminish 'tree-sitter-hl-mode ""))

   (with-eval-after-load 'tree-sitter
     (diminish 'tree-sitter-mode ""))

   (with-eval-after-load 'yasnippet
     (diminish 'yas-minor-mode ""))
   (with-eval-after-load 'company
     (diminish 'company-mode ""))

   (with-eval-after-load 'lsp-mode
     (diminish 'lsp-mode "LSP"))

   (with-eval-after-load 'which-key
     (diminish 'which-key-mode ""))

   (with-eval-after-load 'persp-mode
     (diminish 'persp-mode `(:eval persp-lighter))))

  (add-hook
   'after-load-functions
   (prog1
       (defun re-arrange-minor-mode-alist (&rest _)
         (cl-loop with modes =
                  '(
                    ;; order in which you want the minor mode lighters to
                    ;; appear
                    persp-mode
                    projectile-mode
                    lsp-mode
                    ;; hs-minor-mode
                    ;; outline-minor-mode
                    ;; To get the current order of entries, and what to plug in
                    ;; here do `M-x pp-eval-expression RET (mapcar #'car minor-mode-alist)'
                    )
                  for mode in (nreverse modes)
                  for mode-line-entry = (assq mode minor-mode-alist)
                  when mode-line-entry do
                  (assq-delete-all mode minor-mode-alist)
                  (add-to-list 'minor-mode-alist mode-line-entry)))
     (re-arrange-minor-mode-alist)))

  ;; Clear locally set mode line format -- early buffers such as messages may not be updated with the correct modeline!
  (kzk/after-init
   (dolist (b (buffer-list))
     (when (buffer-local-boundp 'mode-line-format b)
       (with-current-buffer b
         (kill-local-variable 'mode-line-format))))))
