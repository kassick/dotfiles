(defvar consult--source-same-purpose
    `(:name "Same Purpose"
            :narrow   ?p
            :category buffer
            :face     consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :default  t
            :items    ,#'(lambda ()
                           (let ((purpose (purpose-buffer-purpose (current-buffer))))
                             (mapcar #'buffer-name (purpose-buffers-with-purpose purpose))))))
