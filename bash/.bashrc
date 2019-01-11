# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

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

if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    C_IS_ON=true
    C_BOLD="\[\e[1m\]"
    C_DEF="\[\e[0m\]"
    C_BLACK="\[$(/usr/bin/tput setaf 0)\]"
    C_RED="\[$(/usr/bin/tput setaf 1)\]"
    C_GREEN="\[$(/usr/bin/tput setaf 2)\]"
    C_YELLOW="\[$(/usr/bin/tput setaf 3)\]"
    C_BLUE="\[$(/usr/bin/tput setaf 4)\]"
    C_MAGENTA="\[$(/usr/bin/tput setaf 5)\]"
    C_CYAN="\[$(/usr/bin/tput setaf 6)\]"
    C_WHITE="\[$(/usr/bin/tput setaf 7)\]"
    C_GRAY="\[$(/usr/bin/tput setaf 8)\]"
    C_OFF="\[$(/usr/bin/tput sgr0)\]"
    C_ERROR="$(/usr/bin/tput setab 1)$(/usr/bin/tput setaf 7)"
    C_ERROR_OFF="$(/usr/bin/tput sgr0)"
fi

function prompt_command {
        local PWDNAME=$PWD
    local TIME=$(date +%R)
    local TTY=${SSH_TTY:-o}
    
        # beautify working firectory name
        if [[ "$HOME" == "$PWD" ]]; then
                PWDNAME="~"
        elif [[ "$HOME" ==  "${PWD:0:${#HOME}}" ]]; then
                PWDNAME="~${PWD:${#HOME}}"
        fi

    # calculate prompt length
        local PS1_length=$((${#TIME}+${#USER}+${#HOSTNAME}+${#TTY}+${#SHLVL}+${#PWDNAME}+13))
        local FILL=

        # if length is greater, than terminal width
        if [[ $PS1_length -gt $COLUMNS ]]; then
                # strip working directory name
                PWDNAME="...${PWDNAME:$(($PS1_length-$COLUMNS+3))}"
        else
                # else calculate fillsize
                local fillsize=$(($COLUMNS-$PS1_length))
                FILL="$C_GRAY"
                while [[ $fillsize -gt 0 ]]; do FILL="${FILL}─"; fillsize=$(($fillsize-1)); done
                FILL="${FILL}${C_OFF}"
        fi
    
    local TIME="${C_CYAN}${TIME}"
    local USER_HOST="${C_BOLD}${C_BLACK}[${C_BLUE}${USER}@${HOSTNAME}${C_BLACK}:${C_DEF}${C_WHITE}${TTY} ${C_GREEN}+${SHLVL}${C_BLACK}${C_BOLD}]"
    local PWDNAME="${C_WHITE}${PWDNAME}"
        # set new color prompt
        PS1="\n>>> ${TIME} ${USER_HOST} ${PWDNAME} ${FILL}\n$ "
}
PROMPT_COMMAND=prompt_command

# enable local dircolors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

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

# set PATH so it includes user's private bin directories
export PATH="$HOME/.cask/bin:$HOME/.bin:$HOME/.local/bin:$PATH"

# set default editor
export EDITOR="$HOME/.bin/edit"

export NO_AT_BRIDGE=1
