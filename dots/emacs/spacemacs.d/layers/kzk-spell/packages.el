(defconst kzk-spell-packages '(flyspell-correct))

(defun kzk-spell/post-init-flyspell-correct ()
  ;; Helm flyspell

  (kzk/after-init
    (general-define-key :keymaps 'flyspell-mode-map
                        "C-M-;" '("Correct Word" . flyspell-correct-wrapper))

    (general-define-key :keymaps 'flyspell-mode-map
                        :prefix dotspacemacs-leader-key
                        :states '(normal motion visual)
                        "S ;" '("Correct Word" . flyspell-correct-wrapper))))
