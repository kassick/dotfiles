;;; packages.el --- persp-mode tweaks                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodrigo Kassick

;; Author: Rodrigo Kassick <kassick@antagorda>
;; Keywords: persp-mode

(defconst kzk-persp-packages
  '(persp-mode))

(defun kzk-persp/post-init-persp-mode ()
  "Sets persp-mode to not recreate window conf or do any initialization on new frames, as this is is annoying"
  (setq  persp-emacsclient-init-frame-behaviour-override (lambda
                                                           (frame &optional new-frame-p) nil)
         persp-init-frame-behaviour nil)
  )
