# Include evironment variables
if [ -f "$HOME/.environment" ]; then
    . "$HOME/.environment"
fi

# Start tmux if
#  - not in a dumb terminal (e.g. used for emacs tramp)
#  - session is interactive
#  - connected using ssh
#  - not in tmux already
# if command -v tmux &> /dev/null && \
#         [[ "$TERM" != "dumb" ]] && \
#         [[ $- == *i* ]] && \
#         [[ -n "$SSH_CONNECTION" ]] && \
#         [[ -z "$TMUX" ]]
# then
#     exec dbus-launch --exit-with-session tmux new -A -D -s "TMUX_SSH"
# fi

# Start WM if we are in the first virtual console
if command -v start-wm &> /dev/null && [ "1" = "$(fgconsole 2>/dev/null)" ]; then
    exec start-wm &> /dev/null
fi

exec bash
