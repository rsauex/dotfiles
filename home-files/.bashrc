# If in dumb terminal -- do nothing
if [[ "$TERM" == "dumb" ]]; then
    return
fi

# Try to run powershell. 
# KLUDGE! Changing login shell to pwsh breaks remote call over ssh and emacs tramp over ssh
if ! shopt -q login_shell; then
    command -v pwsh-preview &>/dev/null && exec pwsh-preview -nologo
    command -v pwsh &>/dev/null && exec pwsh -nologo
fi
