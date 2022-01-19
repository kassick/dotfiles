;;; kzk-evil.el --- Evil settings                    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodrigo Kassick

;; Author: Rodrigo Kassick <kassick@voyager>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(when (not (boundp 'evil-redirect-digit-argument))
  ;;; Evil refactored this function out of existence, causing several other packages to break :(
  ;;; https://github.com/emacs-evil/evil/pull/1519/files , evil-common.el
  ;;; This kludge should be necessary just for a while
  (defmacro evil-redirect-digit-argument (map keys target)
    (warn (concat "evil-redirect-digit-argument is deprecated -- "
                  "see https://github.com/emacs-evil/evil/pull/1519. "
                  "Called with args %S %S %S")
          map keys target)
     `(define-key ,map ,keys ,target)))

(add-hook 'prog-mode-hook (lambda () (evil-matchit-mode t)))
(add-hook 'text-mode-hook (lambda () (evil-matchit-mode t)))

(define-key evil-insert-state-map (kbd "C-v") 'yank)
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<C-escape>") 'evil-execute-in-emacs-state)

;; evilem on \
(evilem-default-keybindings "\\" )

;; {{{
;; keep these around untill https://github.com/PythonNut/evil-easymotion/pull/71 is merged

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
                            ;; (let ((res (call-interactively func)))
                            ;;   (message "calling func %S resulted in %S" func rec)
                            ;;   res)
                            (message "movement at %S" (point))

                            (message "inclue visible %S" include-invisible)

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

;; MAP C-v
;; evil-args
(general-define-key "L" '(evil-forward-arg :which-key "Arg Forward")
                    "H" '(evil-backward-arg :which-key "Arg Backward")
                    "K" '(evil-jump-out-args :which-key "Jump Out of Args")
                    :states 'normal)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;;; missing from spacemacs PR :/
(defalias 'evil-digit-argument-or-evil-beginning-of-visual-line 'evil-beginning-of-visual-line)

(provide 'kzk-evil)
;;; kzk-evil.el ends here
