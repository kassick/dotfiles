__prompt_expand_envrc_annot() {
    if [[ -n "${ENVRC_ANNOT}" ]]; then
        echo -e "\n\e[3m${ENVRC_ANNOT}\e[0m"
    fi
}

set -o PROMPT_SUBST
export PROMPT='$(__prompt_expand_envrc_annot)'"${PROMPT:-}"
