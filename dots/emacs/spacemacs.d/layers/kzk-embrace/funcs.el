(defun kzk/embrace-show-help-p ()
  "Either evil-embrace will set show-help-p or embrace will"
  (or (and (boundp 'evil-embrace-show-help-p) evil-embrace-show-help-p)
      (and (boundp 'embrace-show-help-p) embrace-show-help-p)))

(defun kzk/embrace--store-help-window (&rest args)
  "Set internal variable to the window used by embrace--show-help-buffer"
  (setq kzk/embrace--embrace-help-window
        (if (kzk/embrace-show-help-p)
            (get-buffer-window embrace--help-buffer)
          nil))
  )

(defun kzk/embrace--kill-help-window (&rest args)
  "If the internal window variable is set, delete it"
  (when (and kzk/embrace--embrace-help-window
           (window-valid-p kzk/embrace--embrace-help-window)
           (not (eq (selected-window) kzk/embrace--embrace-help-window)))
      (delete-window kzk/embrace--embrace-help-window)
    )

  (setq kzk/embrace--embrace-help-window nil))

(defun kzk/add-advise-kill-surround-help-window (&rest args)
  ;; KLUDGE: Store help window in a internal variable after calling embrace--show-help-buffer . Kill it after any action of evil surround
  (advice-add 'embrace--show-help-buffer :after 'kzk/embrace--store-help-window)
  (advice-add 'evil-surround-region :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-surround-change :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-surround-delete :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-surround-edit :after 'kzk/embrace--kill-help-window)
  (advice-add 'evil-Surround-edit :after 'kzk/embrace--kill-help-window)
  )

(defun kzk/del-advise-kill-surround-help-window (&rest args)
  (advice-remove 'embrace--show-help-buffer 'kzk/embrace--store-help-window)
  (advice-remove 'evil-surround-region 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-surround-change 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-surround-delete 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-surround-edit 'kzk/embrace--kill-help-window)
  (advice-remove 'evil-Surround-edit 'kzk/embrace--kill-help-window)
  )
