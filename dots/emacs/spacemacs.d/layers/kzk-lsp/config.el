(custom-set-variables

 ;; categorize entries in imenu -- makes it easier to find stuff
 '(lsp-imenu-index-function 'lsp-imenu-create-categorized-index)

 ;; show less documentation lines in minibuffer. the default uses too much
 ;; vertical space
 '(lsp-signature-doc-lines 5)

 ;; trying to avoid a bug where lsp signature inserts the contents of the
 ;; docstring in the current buffer instead of the minibuffer
 '(lsp-signature-function 'lsp-signature-posframe)
 '(lsp-signature-auto-activate '(:on-server-request))

 ;; slightly smaller doc posframe
 '(lsp-ui-doc-max-height 10)

 ;; Always show peek -- ,gd and ,gr already go to where I want, peek is
 ;; supposed to show a UI
 '(lsp-ui-peek-always-show t)

 ;; Show LSP lenses above the line. I don't like it much, but stuff like
 ;; go-generate commands makd it impossible to use the lenses
 '(lsp-lens-place-position 'above-line)

 ;; Prefer system-wide ruff -- repositories frequently will have stale
 ;; installations used in the CI and during make lint. I prefer using the
 ;; newest versions and update the repo as-needed
 ;; '(lsp-ruff-lsp-server-command `(,(expand-file-name "~/.local/bin/ruff-lsp")))

 ;; '(lsp-ruff-lsp-ruff-path `[,(expand-file-name "~/.local/bin/ruff") "ruff"])

 ;; temporary until lsp-mode and ruff server start getting along https://github.com/emacs-lsp/lsp-mode/issues/4547
 ;; '(lsp-ruff-server-command '("ruff" "server" "--preview"))  ;; locally built server is in preview mode ...

 '(lsp-response-time 20) ;; increase timeout because of glab

 '(lsp-pyright-langserver-command "basedpyright")
 '(lsp-pyright-type-checking-mode "standard")

 '(lsp-copilot-server-multi-root nil)
 )

(defcustom kzk/peek-uses-posframe t "If lsp-ui-peek should appear inside a posframe")
(defcustom kzk/max-peek-width nil "Max width of the peek window. When nil, no max width will be applied")
(defcustom kzk/max-peek-width-proportional nil "Max percentage of the frame width that will be occupied by peek. When nil, do not use")
(defcustom kzk/max-peek-width-proportional-min-width 150 "Minimum width on which proportional applies")
