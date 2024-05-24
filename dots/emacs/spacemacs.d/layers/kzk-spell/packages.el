(defconst kzk-spell-packages '(flyspell-correct))

(defun kzk-spell/post-init-flyspell-correct ()
  ;; Helm flyspell

  (kzk/after-init
    (general-define-key :keymaps 'flyspell-mode-map
                        "C-M-;" '(flyspell-correct-wrapper :which-key "Correct Word"))

    (general-define-key :keymaps 'flyspell-mode-map
                        :prefix dotspacemacs-leader-key
                        :states '(normal motion visual)
                        "S ;" '(flyspell-correct-wrapper :which-key "Correct Word"))))
