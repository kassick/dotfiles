(defconst kzk/copilot-chat-buffer-regexp "^ *\\*Copilot[- ][cC]hat\\(.*\\)\\*\\(\\(\\[.*\\]\\)\\|\\(-.*\\)\\)?\\(<.*>\\)?$"
  "The buffer names of copilot chat that must match the coding-assistant window purpose.

Buffer names:
- '*Copilot-Chat*'
- '*Copilot-Chat [model]*'
- ' *Copilot-Chat*'
- '*Copilot-Chat*[prompt]'
- '*Copilot-Chat [model]*[prompt]'
- ' *Copilot-Chat*-123455'  -- used internally by polymode
")

(defconst kzk/copilot-chat-buffer-list-regexp "^ *\\*[Cc]opilot-chat-list\\*$")


(custom-set-variables
 '(copilot-chat-frontend 'org)
 '(lsp-copilot-enabled t)  ;; upstream decided it should be false by default ...
 '(lsp-copilot-version nil) ;; track latest
 '(copilot-idle-delay nil)
 ;; '(copilot-chat-frontend shell-maker)

 )
