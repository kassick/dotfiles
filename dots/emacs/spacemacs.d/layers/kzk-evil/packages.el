;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defconst kzk-evil-packages
  '(general
    evil))

(defun kzk-evil/post-init-general ())


(defun kzk-evil/post-init-evil ()

  (kzk/after-init
   (message "Unleashing universal evil")
   (general-define-key :keymaps '(evil-evilified-state-map-original evil-evilified-state-map)
                       "C-u" 'universal-argument)

   (spacemacs|define-transient-state insert-paste
     :title "Pasting Transient State"
     :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the \
 same text above or below, [_C-v_] creates a visual selection \
 from last paste and exits. Anything else exits."
     :bindings
     ("C-j" yank-pop)
     ("C-k" (lambda () (interactive) (yank-pop -1)))
     ("p" yank)
     ("0" spacemacs//transient-state-0))

   (define-key evil-insert-state-map (kbd "C-v")
               (lambda (&optional arg)
                 (interactive "*P")
                 (setq this-command 'yank)
                 (cond ((spacemacs//evil-mc-paste-transient-state-p)
                        (spacemacs/insert-paste-transient-state/yank))
                       (t (setq this-command 'yank)
                          (yank arg)))))

   (evil-define-key 'insert vterm-mode-map
     (kbd "C-S-v") 'evil-collection-vterm-paste-after)


   (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
   (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
   (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
   (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
   (define-key evil-motion-state-map (kbd "<C-escape>") 'evil-execute-in-emacs-state)))
