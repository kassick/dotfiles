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
 [_C-j_/_C-k_] cycles through yanked text, \
 [_p_] pastes the same text above or below from last paste and exits.\
 Anything else exits."
     :bindings
     ("C-j" yank-pop)
     ("C-k" (lambda () (interactive) (yank-pop -1)))
     ("p" yank)
     ("0" spacemacs//transient-state-0))

   (defun kzk/yank (&optional arg)
     (interactive "*P")
     ;; (evil-set-marker ?\[ (point))
     (setq this-command 'yank)
     (cond (
            ;; Do not use the insert transient state when
            ;; using multiple cursors
            (spacemacs//evil-mc-paste-transient-state-p)
            (spacemacs/insert-paste-transient-state/yank))
           (
            ;; default behaviour: simply call yank
            t (setq this-command 'yank)
            (yank arg)))
     ;; (evil-set-marker ?\] (1- (point)))
     )
   (define-key evil-insert-state-map (kbd "C-v") #'kzk/yank)
   (define-key evil-insert-state-map [remap yank] #'kzk/yank)

   (defun kzk/yank-sets-evil-marks-maybe (orig-fun &rest args)
     (let ((result (apply orig-fun args)))
       (evil-set-marker ?\[ (mark t))
       (evil-set-marker ?\] (1- (point)))
       result))

   (advice-add 'yank :around #'kzk/yank-sets-evil-marks-maybe)
   (advice-add 'yank-pop :around #'kzk/yank-sets-evil-marks-maybe)
   (advice-add 'mouse-yank-primary :around #'kzk/yank-sets-evil-marks-maybe)
   (advice-add 'mouse-yank-secondary :around #'kzk/yank-sets-evil-marks-maybe)



   (evil-define-key 'insert vterm-mode-map
     (kbd "C-S-v") 'evil-collection-vterm-paste-after)


   (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
   (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
   (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
   (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
   (define-key evil-motion-state-map (kbd "<C-escape>") 'evil-execute-in-emacs-state)

   ;; Some change upstream broke vim-style-retain-visual-state-on-shift. To
   ;; add insult over injury, evil-map macro is kinda limited -- no suport for
   ;; special keys such as <escape>, recursion avoidance is limited to the
   ;; first key, etc. Even if we fixed the macro do correctly handle these
   ;; cases, there would still be the issue of the prefix: calling the
   ;; sequence (kbd "<escape>gv<gv") should apply the prefix to what
   ;; operation? evil-exit-visual-state, evil-restore-visual-state,
   ;; evil-shift-left, ...?
   ;; These new operators handle these cases in a more robust way...
   (define-key evil-visual-state-map
               (kbd "<") #'kzk/evil-visual-shift-left)
   (define-key evil-visual-state-map
               (kbd ">") #'kzk/evil-visual-shift-right)

   ))
