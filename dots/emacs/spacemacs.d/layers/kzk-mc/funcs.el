(defun kzk/evil-iedit-from-current-ex-search ()
  "Starts iedit from within the ex search prompt"
  (interactive)

  (let* ((regexp (minibuffer-contents-no-properties))
         (pattern (evil-ex-make-pattern regexp evil-ex-search-case t))
         (iedit-case-sensitive (not (evil-ex-pattern-ignore-case pattern))))
    (when evil-ex-current-buffer
      (with-current-buffer evil-ex-current-buffer
        (with-selected-window (minibuffer-selected-window)
          (require 'iedit)
          (spacemacs/evil-search-clear-highlight)
          (iedit-start regexp 0 (point-max))
          (evil-iedit-state))))
    (exit-minibuffer)))

(defun kzk/evil-iedit-from-last-ex-search ()
  "Starts iedit from the last searched pattern"
  (interactive)
  (when evil-ex-search-pattern
    (let* ((iedit-case-sensitive (not (evil-ex-pattern-ignore-case evil-ex-search-pattern)))
           (regexp (evil-ex-pattern-regex evil-ex-search-pattern)))
      (require 'iedit)
      (spacemacs/evil-search-clear-highlight)
      (iedit-start regexp 0 (point-max))
      (evil-iedit-state)
      (iedit-next-occurrence 1))))
