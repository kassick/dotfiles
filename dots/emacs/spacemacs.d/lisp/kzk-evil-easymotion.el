;; {{{
;; keep these around untill https://github.com/PythonNut/evil-easymotion/pull/71 is merged
(message "tweaking evilem")

(defun evilem--collect (func &optional
                            scope
                            all-windows
                            initial-point
                            sort-key
                            collect-postprocess
                            include-invisible)
  "Repeatedly execute func, and collect the cursor positions into a list"
  (cl-letf ((points nil)
            (point nil)
            (avy-all-windows all-windows)
            ;; make sure the motion doesn't move the window
            (scroll-conservatively 101)
            (smooth-scrolling-mode nil)
            (scroll-margin 0)
            (evil-state 'normal))

    (if (functionp func)
        (avy-dowindows current-prefix-arg
          (save-excursion
            (save-restriction
              (when initial-point
                (goto-char (funcall initial-point)))
              (cl-destructuring-bind (beg . end)
                  (if scope
                      (bounds-of-thing-at-point scope)
                    (cons (point-min)
                          (point-max)))

                ;; trim trailing newline
                (when (= (char-before end) 10)
                  (cl-decf end))

                (narrow-to-region (max beg (window-start))
                                  (min end (window-end))))
              (while (and (ignore-errors
                            (setq this-command func
                                  last-command func)
                            (call-interactively func)

                            (unless include-invisible
                              (let ((ov (car (overlays-at (point)))))
                                (while (and ov (member
                                                'invisible
                                                (overlay-properties ov)))
                                  (goto-char (overlay-end ov))
                                  ;; This is a bit of a hack, since we
                                  ;; can't guarantee that we will end
                                  ;; up at the same point if we start
                                  ;; at the end of the invisible
                                  ;; region vs. looping through it.
                                  (call-interactively func)
                                  (setq ov (car (overlays-at (point)))))))
                            t)
                          (setq point (cons (point) (get-buffer-window)))
                          (not (member point points))
                          (push point points))))))
      (setq points (cl-remove-duplicates
                    (cl-mapcan (lambda (f)
                                (evilem--collect f scope all-windows))
                              func))))
    (funcall (or collect-postprocess
                #'evilem--default-collect-postprocess)
            points)))

(eval-and-compile
  (defun evilem--compute-inclusivity (funcs)
    (cond ((symbolp funcs) `(setq evil-this-type
                                  ',(evil-get-command-property funcs :type)))

          ((and (listp funcs) (= (length funcs) 1)) (evilem--compute-inclusivity (car funcs)))

          ((and (listp funcs) (= (length funcs) 2) (eq (car funcs) 'function)) (evilem--compute-inclusivity (cdr funcs))))))

(evilem-make-motion
evilem-motion-forward-word-begin #'evil-forward-word-begin
:scope 'line)

(evilem-make-motion
evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin
:scope 'line)

(evilem-make-motion
evilem-motion-forward-word-end #'evil-forward-word-end
:scope 'line)

(evilem-make-motion
evilem-motion-forward-WORD-end #'evil-forward-WORD-end
:scope 'line)

(evilem-make-motion
evilem-motion-backward-word-begin #'evil-backward-word-begin
:scope 'line)

(evilem-make-motion
evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin
:scope 'line)

(evilem-make-motion
evilem-motion-backward-word-end #'evil-backward-word-end
:scope 'line)

(evilem-make-motion
evilem-motion-backward-WORD-end #'evil-backward-WORD-end
:scope 'line)

(evilem-make-motion
evilem-motion-next-line #'next-line
:pre-hook (setq evil-this-type 'line)
:bind ((temporary-goal-column (current-column))
        (line-move-visual nil)))

(evilem-make-motion
evilem-motion-previous-line #'previous-line
:pre-hook (setq evil-this-type 'line)
:bind ((temporary-goal-column (current-column))
        (line-move-visual nil)))

(evilem-make-motion
evilem-motion-next-visual-line #'next-line
:pre-hook (setq evil-this-type 'line)
:bind ((temporary-goal-column (current-column))
        (line-move-visual t)))

(evilem-make-motion
evilem-motion-previous-visual-line #'previous-line
:pre-hook (setq evil-this-type 'line)
:bind ((temporary-goal-column (current-column))
        (line-move-visual t)))

(evilem-make-motion
evilem-motion-find-char-to #'evil-repeat-find-char
:pre-hook (save-excursion
            (setq evil-this-type 'inclusive)
            (call-interactively #'evil-find-char-to))
:bind ((evil-cross-lines t)))

(evilem-make-motion
evilem-motion-find-char-to-backward #'evil-repeat-find-char
:pre-hook (save-excursion
            (setq evil-this-type 'exclusive)
            (call-interactively #'evil-find-char-to-backward))
:bind ((evil-cross-lines t)))

(evilem-make-motion
evilem-motion-find-char #'evil-repeat-find-char
:pre-hook (save-excursion
            (setq evil-this-type 'inclusive)
            (call-interactively #'evil-find-char))
:bind ((evil-cross-lines t)))

(evilem-make-motion
evilem-motion-find-char-backward #'evil-repeat-find-char
:pre-hook (save-excursion
            (setq evil-this-type 'exclusive)
            (call-interactively #'evil-find-char-backward))
:bind ((evil-cross-lines t)))

(evilem-make-motion
evilem-motion-backward-section-begin #'evil-backward-section-begin
:pre-hook (setq evil-this-type 'line))

(evilem-make-motion
evilem-motion-backward-section-end #'evil-backward-section-end
:pre-hook (setq evil-this-type 'line))

(evilem-make-motion
evilem-motion-forward-section-begin #'evil-forward-section-begin
:pre-hook (setq evil-this-type 'line))

(evilem-make-motion
evilem-motion-forward-section-end #'evil-forward-section-end
:pre-hook (setq evil-this-type 'line))

(evilem-make-motion
evilem-motion-backward-sentence-begin #'evil-backward-sentence-begin)

(evilem-make-motion
evilem-motion-forward-sentence-begin #'evil-forward-sentence-begin)

(evilem-make-motion
evilem-motion-search-next #'evil-search-next
:bind (((symbol-function #'isearch-lazy-highlight-update)
        #'ignore)
        (search-highlight nil)))

(evilem-make-motion
evilem-motion-search-previous #'evil-search-previous
:bind (((symbol-function #'isearch-lazy-highlight-update)
        #'ignore)
        (search-highlight nil)))

(evilem-make-motion
evilem-motion-search-word-forward #'evil-search-word-forward
:bind (((symbol-function #'isearch-lazy-highlight-update)
        #'ignore)
        (search-highlight nil)))

(evilem-make-motion
evilem-motion-search-word-backward #'evil-search-word-backward
:bind (((symbol-function #'isearch-lazy-highlight-update)
        #'ignore)
        (search-highlight nil)))

(evilem-make-motion
evilem-motion-previous-line-first-non-blank #'evil-previous-line-first-non-blank)

(evilem-make-motion
evilem-motion-next-line-first-non-blank #'evil-next-line-first-non-blank)

;; ;; fix some evilem motions not being inclusive
;; (evilem-make-motion
;;  evilem-motion-forward-word-end #'evil-forward-word-end
;;  :scope 'line
;;  :pre-hook (setq evil-this-type 'inclusive))

;; (evilem-make-motion
;;  evilem-motion-forward-WORD-end #'evil-forward-WORD-end
;;  :scope 'line
;;  :pre-hook (setq evil-this-type 'inclusive))

;; (evilem-make-motion
;;  evilem-motion-backward-word-begin #'evil-backward-word-begin
;;  :scope 'line
;;  :pre-hook (setq evil-this-type 'inclusive))

;; (evilem-make-motion
;;  evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin
;;  :scope 'line
;;  :pre-hook (setq evil-this-type 'inclusive))


;; (evilem-make-motion
;;  evilem-motion-backward-word-end #'evil-backward-word-end
;;  :scope 'line
;;  :pre-hook (setq evil-this-type 'inclusive))

;; (evilem-make-motion
;;  evilem-motion-backward-WORD-end #'evil-backward-WORD-end
;;  :scope 'line
;;  :pre-hook (setq evil-this-type 'inclusive))

;; ;; increase scope of word jumps
;; (evilem-make-motion
;;  evilem-motion-forward-word-begin #'evil-forward-word-begin)

;; (evilem-make-motion
;;  evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin)

;; }}}


(provide 'kzk-evil-easymotion)
