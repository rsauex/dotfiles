# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# if [ -f "$HOME/.bashrc" ]; then
#     . "$HOME/.bashvars"
# fi

# setup color variables
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    C_IS_ON=true
    C_BACK="\[$(/usr/bin/tput setaf 0)\]"
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

declare -A PATH_ALIASES

PATH_ALIASES=( ["$HOME"]="~" )

function prompt_command {
    declare -A path_aliases
    local path_aliases=( ["$HOME"]="~" )
    
    # current dir
    local PWDNAME=$PWD
    # current time
    local TIME=`date +%R`

    # apply path aliases
    for i in "${!path_aliases[@]}"; do
        if [ "$i" ==  "${PWD:0:${#i}}" ]; then
            PWDNAME="${path_aliases[$i]}${PWD:${#i}}"
        fi
    done

    local PS1_without_colors=">>> ${TIME} [${USER}@${HOSTNAME}:${SSH_TTY:-o} +${SHLVL}] ${PWDNAME} "
    
    # calculate fillsize
    local fillsize=$(($COLUMNS-${#PS1_without_colors}))
    
    local FILL=$C_WHITE
    while [ $fillsize -gt 0 ]; do FILL="${FILL}─"; fillsize=$(($fillsize-1)); done
    FILL="${FILL}${C_OFF}"
        
    # set new color prompt
    PS1="\n${C_GREEN}>>> ${C_BLUE}${TIME} ${C_GRAY}[${C_CYAN}${USER}@${HOSTNAME}${C_GRAY}:${C_WHITE}${SSH_TTY:-o} ${C_GREEN}+${SHLVL}${C_GRAY}] ${C_WHITE}${PWDNAME}${C_OFF} ${FILL}\n\$ "
}

PROMPT_COMMAND=prompt_command

# enable color support of ls and also add handy aliases
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

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

## Ubuntu 18 hacks
export JAVA_HOME=/software/Linux/x86_64/Java/JDK
export PATH=/software/Linux/x86_64/Java/JDK/bin:$PATH
export MAVEN_HOME=/usr

## Enable direnv
eval "$(direnv hook bash)"

## Enable xsh
# source ~/.xsh

