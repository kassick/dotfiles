if [[ "$PROMPT" =~ ^$'\n' ]]; then
    __spacer=$'\n'
fi
export PROMPT="$__spacer"$'%{\e[3m%}${ENVRC_ANNOT}%{\e0%}'"${PROMPT:-}"
unset __spacer
