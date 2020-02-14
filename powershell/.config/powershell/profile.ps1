function Test-LoginShell() {
    (ps -p $PID -o cmd=) -match "^-pwsh"
}
