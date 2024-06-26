;; -*- mode: emacs-lisp; lexical-binding: t -*-

(with-eval-after-load 'evil
  (evil-define-operator kzk/evil-visual-shift-right (beg end &optional count preserve-empty)
    :type line
    :move-point nil ; point is moved according to `evil-start-of-line' and state
    (interactive "<r><vc>")

    (when (evil-visual-state-p)
      (evil-exit-visual-state)
      (evil-visual-restore)
      (evil-shift-right beg end count preserve-empty)
      (evil-visual-restore)))


  (evil-define-operator kzk/evil-visual-shift-left (beg end &optional count preserve-empty)
    :type line
    :move-point nil ; point is moved according to `evil-start-of-line' and state
    (interactive "<r><vc>")

    (when (evil-visual-state-p)
      (evil-exit-visual-state)
      (evil-visual-restore)
      (evil-shift-left beg end count preserve-empty)
      (evil-visual-restore))))
