# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.

export XDG_CONFIG_HOME=$HOME/.config

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# start X11 if in tty1
[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx &> /dev/null
