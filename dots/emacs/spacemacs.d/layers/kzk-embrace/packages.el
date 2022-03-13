(defconst kzk-embrace-packages '(evil-embrace))

(defun kzk-embrace/init-evil-embrace ()
  (advice-add 'evil-embrace-enable-evil-surround-integration
              :after 'kzk/add-advise-kill-surround-help-window)
  (advice-add 'evil-embrace-disable-evil-surround-integration
              :after 'kzk/del-advise-kill-surround-help-window)
  (evil-embrace-enable-evil-surround-integration)
  )
