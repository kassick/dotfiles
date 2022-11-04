if [ -n "$SSH_CONNECTION" ]; then
    EDITOR='emacsclient -t --alternate-editor="" '
else
    EDITOR='gtk-launch emacs'
fi

export EDITOR
alias e=$EDITOR

# Fix for emacs internal terminal
if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unset precmd_functions
    unset preexec_functions
    PS1='$ '
fi
