(defun helm-esw/show-buffer (candidate)
  (set-window-buffer (esw/select-window nil t t) (get-buffer candidate)))


(defun helm-esw/find-file-splitted-window (filename)
  "Use esw to select the target window for filename"
  (let* ((buffer (find-file-noselect filename))
         (new-window (esw/select-window nil t t)))
    (set-window-buffer new-window buffer)
    (select-window new-window)

    buffer))

(defun helm-esw/ag-find-file (candidate)
  "Selects a target window with esw before finding the file"
  (helm-ag--find-file-action candidate 'helm-esw/find-file-splitted-window (helm-ag--search-this-file-p)))

(defun helm-esw/run-ag-find-file ()
  "Show the selected match in the selected window"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/ag-find-file)))

(defun helm-esw/find-file (candidate)
  (set-window-buffer (esw/select-window nil t t) (find-file-noselect candidate)))

(defun helm-esw/run-show-buffer ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/show-buffer)))

(defun helm-esw/run-find-file ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/find-file)))

(defun kzk/setup-helm-esw ()
  ;; find file
  (add-to-list 'helm-find-files-actions '("Find file in in new splited window `C-c w'" . helm-esw/find-file ) t)
  (define-key helm-find-files-map (kbd "C-c w") 'helm-esw/run-find-file)

  (with-eval-after-load 'projectile
    (helm-projectile-on)
    ;; projectille ff is in a different map
    (define-key helm-projectile-find-file-map (kbd "C-c w") 'helm-esw/run-find-file))

  ;; file type actions
  (add-to-list 'helm-type-file-actions '("Find file in in new splited window `C-c w'" . helm-esw/find-file ) t)
  (define-key helm-generic-files-map (kbd "C-c w") 'helm-esw/run-find-file)

  ;; buffers
  (add-to-list 'helm-type-buffer-actions
               '("Display buffer(s) in new splited window `C-c w'" . helm-esw/show-buffer) t)
  (define-key helm-buffer-map (kbd "C-c w") 'helm-esw/run-show-buffer)

  ;; helm ag
  (with-eval-after-load 'helm-ag
    (add-to-list 'helm-ag--actions
                 '("Find match in in new splited window `C-c w'" . helm-esw/ag-find-file ) t)
    (define-key helm-ag-map (kbd "C-c w") 'helm-esw/run-ag-find-file)))
