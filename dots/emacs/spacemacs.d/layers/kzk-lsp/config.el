(custom-set-variables

 ;; categorize entries in imenu -- makes it easier to find stuff
 '(lsp-imenu-index-function 'lsp-imenu-create-categorized-index)

 ;; show less documentation lines in minibuffer. the default uses too much
 ;; vertical space
 '(lsp-signature-doc-lines 5)
 )
