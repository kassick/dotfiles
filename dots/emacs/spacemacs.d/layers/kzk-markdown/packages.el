(defconst kzk-markdown-packages
  '(markdown-mode))

(defun kzk-markdown/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook
            (lambda ()
              (embrace-add-pair ?_ "_" "_")
              (embrace-add-pair ?* "*" "*")))

  (with-eval-after-load 'markdown-mode
    (advice-add 'markdown-follow-thing-at-point :around #'kzk/markdown-follow-at-point)))
