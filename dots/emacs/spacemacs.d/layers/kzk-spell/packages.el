(defconst kzk-spell-packages '(flyspell-correct))

(defun kzk-spell/post-init-flyspell-correct ()
  ;; Helm flyspell


  (kzk/after-init
   (message "Defining keys for flyspell-mode")
   (define-key global-map
               (kbd "C-M-;") '("Correct Word" . flyspell-correct-wrapper))
   (spacemacs/set-leader-keys
     "S ;" '("Correct Word" . flyspell-correct-wrapper)))
  )
