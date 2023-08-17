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

# inside emacs vterm?
if [[ "$INSIDE_EMACS" ]]
then
    if [ -n "$EMACS_VTERM_PATH" ] && [ -f "$EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh" ]
    then
        source "$EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh"
    fi

    # override to ensure we do not launch terminal emacs when not expected...
    # Running terminal emacs inside vterm inside emacs leads to several
    # issues...
    export GIT_EDITOR="emacsclient"
    export EDITOR="emacsclient"
    alias e="emacsclient"
fi
