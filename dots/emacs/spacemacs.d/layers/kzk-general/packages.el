(defconst kzk-general-packages '(general))

(defun kzk-general/init-general ()
  (message "General Evil unleashed"))

(kzk/after-init
 (general-evil-setup))
