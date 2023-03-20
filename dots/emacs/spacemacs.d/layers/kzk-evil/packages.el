(defconst kzk-evil-packages
  '(general
    evil))

(defun kzk-evil/post-init-general ())

(defun kzk-evil/paste (&optional COUNT REGISTER)
  (interactive "*P")
  (spacemacs/evil-mc-paste-before COUNT REGISTER)
  (right-char))

(defun kzk-evil/post-init-evil ()

  (kzk/after-init
   (message "Unleashing universal evil")
   (general-define-key :keymaps '(evil-evilified-state-map-original evil-evilified-state-map)
                       "C-u" 'universal-argument)
   (define-key evil-insert-state-map (kbd "C-v") 'kzk-evil/paste)
   (evil-define-key 'insert vterm-mode-map
     (kbd "C-S-v") 'evil-collection-vterm-paste-after)


   (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
   (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
   (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
   (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
   (define-key evil-motion-state-map (kbd "<C-escape>") 'evil-execute-in-emacs-state)))
