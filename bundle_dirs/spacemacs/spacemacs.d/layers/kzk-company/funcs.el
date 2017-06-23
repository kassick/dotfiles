
;; ALWAYS show the tooltip. It's more vim-like and I
;; prefer it this way
;;(defun company--show-inline-p () nil)

;; Predicate to insert completion
(defun kzk/company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)))

;; My take on docbuffers
  (defun kzk/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (company-doc-buffer (company-call-backend 'meta selected)))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))

  ;; A doc-buffer frontend that updates the current docbuffer
  (defun kzk/company-doc-buffer-frontend (command)
    "`company-doc-buffer-frontend' -- shows documentation for current candidate"
    (interactive)
    (when (eq command 'post-command)
      (let* ((selected (nth company-selection company-candidates))
             (meta-text (company-call-backend 'meta selected))
             (doc-buffer (or (company-call-backend 'doc-buffer selected)
                             (company-doc-buffer ""))))
        (if doc-buffer
            (let* ((doc-buffer-win (get-buffer-window doc-buffer)))
              (when doc-buffer-win
                  (with-current-buffer doc-buffer
                    (progn
                      (goto-char (point-min))
                      (insert (concat meta-text "\n\n"))))
                  (display-buffer doc-buffer t)
                  ;(minimize-window doc-buffer-win)
                  (fit-window-to-buffer doc-buffer-win 10 2 nil nil t)))))))
