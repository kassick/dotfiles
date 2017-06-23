;;; Fonts setup
;; Fonts used:
;; - Source Code Pro (https://github.com/adobe-fonts/source-code-pro)
;; - Fira Sans (https://github.com/mozilla/Fira/)
(defun kzk/setup-main-fonts (default-height variable-pitch-height)
  "Set up default fonts.

Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height variable-pitch-height
                      :weight 'regular))

(defun kzk/adjust-font-size ()
  ;; Dinamically change font size based upon screen resolution
  (if (display-graphic-p)
      (if (> (display-pixel-width) 1800) ; Has X, query pixel width
          (kzk/setup-main-fonts 130 140)
        (kzk/setup-main-fonts 120 130))
    (mu-setup-main-fonts 130 140) ; no X yet, maybe we are a server starting?
                                  ; Default to 130 140
    )
  )

