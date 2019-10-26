# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.

export XDG_CONFIG_HOME=$HOME/.config

# set PATH so it includes user's private bin directories
export PATH="$HOME/.bin:$HOME/.local/bin:$PATH"

# set default editor
export EDITOR="$HOME/.bin/editor"

# SSH agent
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"

# include evironment variables
if [ -f "$HOME/.environment" ]; then
    . "$HOME/.environment"
fi

# if running bash include .bashrc if it exists
if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# start X11 if in tty1
[[ "$(fgconsole 2>/dev/null)" == 1 ]] && exec startx &> /dev/null
