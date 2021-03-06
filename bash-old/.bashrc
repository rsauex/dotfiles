# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# If in dumb terminal, don't do anything
[[ "$TERM" == "dumb" ]] && return

# Try to run powershell. 
# KLUDGE! Changing login shell to pwsh breaks remote call over ssh and emacs tramp over ssh
if [[ -z "${IN_NIX_SHELL}" ]]; then
    command -v pwsh-preview &>/dev/null && exec pwsh-preview -nologo
    command -v pwsh &>/dev/null && exec pwsh -nologo
fi

# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# colors
. ~/.config/bash/colors

# prompt
. ~/.config/bash/prompt

PROMPT_COMMAND=fancy_prompt_command

# enable local dircolors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# prompt hook helper
posthook() {
    export PROMPT_COMMAND="$1;$PROMPT_COMMAND"
}

# update environment variables when using tmux
_update_env_tmux() {
    if [ ! -z "$TMUX" ]; then
        eval "$(tmux show-environment -s)"
    fi
}
posthook _update_env_tmux

# loading aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

eval "$(direnv hook bash)"
