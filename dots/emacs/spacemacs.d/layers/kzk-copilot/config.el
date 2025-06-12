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

(defvar kzk/copilot-chat-map (make-sparse-keymap)
  "The Copiloting keymap")

(defvar kzk/aider-map (make-sparse-keymap)
  "The Aider keymep")

(kzk/after-init
 (define-key global-map (kbd "C-&") `("Copilot" . ,kzk/copilot-chat-map))
 (spacemacs/set-leader-keys "&" `("Copilot" . ,kzk/copilot-chat-map))

 (define-key global-map (kbd "C-%") `("Aider" . ,kzk/aider-map))
 (spacemacs/set-leader-keys "%" `("Aider" . ,kzk/aider-map)))


(custom-set-variables
 '(copilot-chat-frontend 'org)
 `(aider-program ,(expand-file-name "~/.local/bin/ifood-aider-helper"))
 '(aider-args '(
                "--model" "litellm_proxy/claude-3-7-sonnet-20250219-v1.0"
                "--editor-model" "litellm_proxy/claude-3-7-sonnet-20250219-v1.0"
                "--weak-model" "litellm_proxy/ifood_default"
                "--edit-format" "diff"
                "--editor" "emacsclient -c"
                "--chat-lang" "en_US"
                "--no-show-model-warnings"))
 '(aider-popular-models '(;; curl -vvv -X GET https://generative-ai-platform.ifood-sandbox.com.br/api/v2/v1/models | jq '"litellm_proxy/\(.data[] | .id)"'|sort
                          ;; not all these are actually available
                          "litellm_proxy/babbage-002"
                          "litellm_proxy/chatgpt-4o-latest"
                          "litellm_proxy/claude-3-5-haiku-20241022-v1.0"
                          "litellm_proxy/claude-3-5-sonnet-20240620-v1.0"
                          "litellm_proxy/claude-3-5-sonnet-20241022-v2.0"
                          "litellm_proxy/claude-3-7-sonnet-20250219-v1.0"
                          "litellm_proxy/claude-3-haiku-20240307-v1.0"
                          "litellm_proxy/claude-3-opus-20240229-v1.0"
                          "litellm_proxy/claude-3-sonnet-20240229-v1.0"
                          "litellm_proxy/claude-instant-v1"
                          "litellm_proxy/claude-opus-4-20250514-v1.0"
                          "litellm_proxy/claude-sonnet-4-20250514-v1.0"
                          "litellm_proxy/claude-v2"
                          "litellm_proxy/claude-v2.1"
                          "litellm_proxy/cohere-rerank-v3.5"
                          "litellm_proxy/command-light-text-v14"
                          "litellm_proxy/command-r-plus-v1.0"
                          "litellm_proxy/command-r-v1.0"
                          "litellm_proxy/command-text-v14"
                          "litellm_proxy/criteria/cost"
                          "litellm_proxy/criteria/latency"
                          "litellm_proxy/criteria/quality"
                          "litellm_proxy/dall-e-2"
                          "litellm_proxy/dall-e-3"
                          "litellm_proxy/davinci-002"
                          "litellm_proxy/deepseek-r1"
                          "litellm_proxy/deepseek-v3"
                          "litellm_proxy/embed-english-v3"
                          "litellm_proxy/embed-multilingual-v3"
                          "litellm_proxy/ft:babbage-002"
                          "litellm_proxy/ft:davinci-002"
                          "litellm_proxy/ft:gpt-4-0613"
                          "litellm_proxy/ft:gpt-4o-2024-08-06"
                          "litellm_proxy/ft:gpt-4o-2024-11-20"
                          "litellm_proxy/ft:gpt-4o-mini-2024-07-18"
                          "litellm_proxy/gemini-1.5-flash"
                          "litellm_proxy/gemini-1.5-pro"
                          "litellm_proxy/gemini-2.0-flash"
                          "litellm_proxy/gemini-2.0-flash-lite"
                          "litellm_proxy/gemini-2.5-flash-preview-04-17"
                          "litellm_proxy/gemini-2.5-pro-exp-03-25"
                          "litellm_proxy/gemini-2.5-pro-experimental"
                          "litellm_proxy/gemini-2.5-pro-preview-05-06"
                          "litellm_proxy/gemini-experimental"
                          "litellm_proxy/gpt-3.5-turbo"
                          "litellm_proxy/gpt-4-0125-preview"
                          "litellm_proxy/gpt-4-0613"
                          "litellm_proxy/gpt-4.1"
                          "litellm_proxy/gpt-4-1106-preview"
                          "litellm_proxy/gpt-4.1-mini"
                          "litellm_proxy/gpt-4.1-nano"
                          "litellm_proxy/gpt-4o"
                          "litellm_proxy/gpt-4o-2024-05-13"
                          "litellm_proxy/gpt-4o-2024-08-06"
                          "litellm_proxy/gpt-4o-2024-11-20"
                          "litellm_proxy/gpt-4o-audio-preview"
                          "litellm_proxy/gpt-4o-audio-preview-2024-10-01"
                          "litellm_proxy/gpt-4o-mini"
                          "litellm_proxy/gpt-4o-mini-2024-07-18"
                          "litellm_proxy/gpt-4o-mini-audio-preview-2024-12-17"
                          "litellm_proxy/gpt-4o-mini-realtime-preview"
                          "litellm_proxy/gpt-4o-mini-realtime-preview-2024-12-17"
                          "litellm_proxy/gpt-4o-mini-transcribe"
                          "litellm_proxy/gpt-4o-realtime-preview"
                          "litellm_proxy/gpt-4o-realtime-preview-2024-10-01"
                          "litellm_proxy/gpt-4o-realtime-preview-2024-12-17"
                          "litellm_proxy/gpt-4o-transcribe"
                          "litellm_proxy/gpt-4-turbo"
                          "litellm_proxy/gpt-4-turbo-2024-04-09"
                          "litellm_proxy/gpt-4-turbo-preview"
                          "litellm_proxy/gpt-image-1"
                          "litellm_proxy/ifood_cheap"
                          "litellm_proxy/ifood_cheap-thinking"
                          "litellm_proxy/ifood_default"
                          "litellm_proxy/ifood_smart"
                          "litellm_proxy/ifood_smart-thinking"
                          "litellm_proxy/llama3-1-70b-instruct-v1.0"
                          "litellm_proxy/llama3-1-8b-instruct-v1.0"
                          "litellm_proxy/llama3-2-11b-instruct-v1.0"
                          "litellm_proxy/llama3-2-1b-instruct-v1.0"
                          "litellm_proxy/llama3-2-3b-instruct-v1.0"
                          "litellm_proxy/llama3-2-90b-instruct-v1.0"
                          "litellm_proxy/llama3-3-70b-instruct-v1.0"
                          "litellm_proxy/llama3-70b-instruct-v1.0"
                          "litellm_proxy/llama3-8b-instruct-v1.0"
                          "litellm_proxy/llama4-maverick-17b"
                          "litellm_proxy/llama4-scout-17b"
                          "litellm_proxy/llama-guard-4-12b"
                          "litellm_proxy/mistral-7b-instruct-v0.2"
                          "litellm_proxy/mistral-8x7b-instruct-v0.1"
                          "litellm_proxy/mistral-large-2402-v1.0"
                          "litellm_proxy/mistral-small-2402-v1.0"
                          "litellm_proxy/nova-canvas-v1.0"
                          "litellm_proxy/nova-lite-v1.0"
                          "litellm_proxy/nova-micro-v1.0"
                          "litellm_proxy/nova-pro-v1.0"
                          "litellm_proxy/nova-reel-v1.0"
                          "litellm_proxy/o1"
                          "litellm_proxy/o1-2024-12-17"
                          "litellm_proxy/o1-mini"
                          "litellm_proxy/o1-mini-2024-09-12"
                          "litellm_proxy/o1-preview"
                          "litellm_proxy/o1-preview-2024-09-12"
                          "litellm_proxy/o3"
                          "litellm_proxy/o3-mini"
                          "litellm_proxy/o4-mini"
                          "litellm_proxy/omni-moderation-2024-09-26"
                          "litellm_proxy/omni-moderation-latest"
                          "litellm_proxy/prosus_lcm_gpt-4o"
                          "litellm_proxy/prosus_lcm_gpt-4o-mini"
                          "litellm_proxy/qwen3-235bx22b"
                          "litellm_proxy/qwen-qwq-32b"
                          "litellm_proxy/text-embedding-3-large"
                          "litellm_proxy/text-embedding-3-small"
                          "litellm_proxy/text-embedding-ada-002"
                          "litellm_proxy/text-moderation-007"
                          "litellm_proxy/text-moderation-latest"
                          "litellm_proxy/text-moderation-stable"
                          "litellm_proxy/titan-embed-image-v1"
                          "litellm_proxy/titan-embed-text-v1"
                          "litellm_proxy/titan-embed-text-v2.0"
                          "litellm_proxy/titan-image-generator-v1"
                          "litellm_proxy/titan-image-generator-v2.0"
                          "litellm_proxy/titan-text-express-v1"
                          "litellm_proxy/titan-text-lite-v1"
                          "litellm_proxy/titan-text-premier-v1.0"
                          "litellm_proxy/tts-1"
                          "litellm_proxy/tts-1-hd"
                          "litellm_proxy/whisper-1"
                          ))
 '(lsp-copilot-enabled t)  ;; upstream decided it should be false by default ...
 '(lsp-copilot-version nil) ;; track latest
 '(copilot-idle-delay nil)
 ;; '(copilot-chat-frontend shell-maker)

 )
