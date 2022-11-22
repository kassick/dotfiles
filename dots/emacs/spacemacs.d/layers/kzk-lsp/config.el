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
 )
