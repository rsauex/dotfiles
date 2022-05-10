#!/usr/bin/env bash

## --instance|--class|--title <value> none|<workspace> none|<output> <program> <arguments>...

# Set $3 'none' to stay on current workspace.
# Set $4 'none' to stay on current output.

if [[ $# -lt 5 ]]; then
    echo '--instance|--class|--title <value> none|<workspace> none|<output> <program> <arguments>...'
    exit 1
fi

prepare_exec_str() {
    for value in "$@"; do
        case "$value" in  
            *\ * )  echo -n "\"$value\" " ;;
            *)      echo -n "$value "     ;;
        esac
    done
}

# Attempt to match the running program and obtain its window id.

case "$1" in
    --instance)  option="--classname" ;;
    --class)     option="--class"     ;;
    --title)     option="--name"      ;;
    *)           echo 'First argument must be either --instance --class or --title'; exit 2 ;;
esac

W=$(xdotool search "$option" "$2" | head -1)

if [[ -z "$W" ]]; then
    # Change to the specified workspace.
    if [[ "$3" != 'none' ]]; then
        i3-msg "workspace $3" > /dev/null
    fi
    exec_str=( "${@:5}" )
    # Launch the program.
    i3-msg "exec `prepare_exec_str \"${exec_str[@]}\"`" > /dev/null
    # Change to the specified output when the window appears.
    if [[ "$4" != 'none' ]]; then
        sleep 0.3
        i3-msg "move workspace to output $4" > /dev/null
    fi
else
    # Focus the workspace.
    if [[ "$3" != 'none' ]]; then
        i3-msg "workspace $3" > /dev/null
    fi
    # Focus the program.
    i3-msg "[${1:2}=\"$2\" workspace=\"__focused__\"] focus" > /dev/null
fi
