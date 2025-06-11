(defvar kzk/pupo-managed-buffers nil "buffers that have been managed by pupo/popwin")


(custom-set-variables

 ;;; enable horizontal scrolling
 '(mouse-wheel-tilt-scroll t)

 ;;; keep consistent with the rest of the system
 '(mouse-wheel-flip-direction t)

 '(winum-scope 'frame-local)

 ;; Thresholds for split are a bit aggressive these days... This, combined
 ;; with the config for purpose in packages.el, aims to improve
 ;; switch-to-buffer-other-window flows -- with the default values, it would
 ;; frequently try to find a window in a completely different frame, because
 ;; the current frame was already too busy (i.e. an edit window and a
 ;; compilation window at the bottom ugh)
 '(split-width-threshold 130)
 '(split-height-threshold 60)
 )
