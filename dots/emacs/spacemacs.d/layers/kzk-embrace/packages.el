(defconst kzk-embrace-packages '(evil-embrace evil-surround magit))

(defun kzk-embrace/init-evil-embrace ()
  (advice-add 'evil-embrace-enable-evil-surround-integration
              :after 'kzk/add-advise-kill-surround-help-window)
  (advice-add 'evil-embrace-disable-evil-surround-integration
              :after 'kzk/del-advise-kill-surround-help-window)
  (evil-embrace-enable-evil-surround-integration)
  )

(defun kzk-embrace/post-init-evil-surround ()
  ;; pass
  )

(defun kzk-embrace/post-init-magit ()
  ;; force disable evil-surround in magit as it shadows s for staging
  (add-hook 'magit-mode-hook (lambda () (evil-surround-mode -1))))
