#!/bin/bash

### TODO: focus if not focused and is not in scratchpad
### TODO: rewrite in perl6 with better i3-ipc

## --instance|--class|--title <value> <program> <arguments>...

if [[ $# -lt 3 ]]; then
    echo '--instance|--class|--title <value> <program> <arguments>...'
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

window_in_scratch() {
    i3-msg -t get_tree | jq "[recurse(.nodes[], .floating_nodes[]) \
                                | select(.window_properties | [.$1 == \"$2\"] | all)] \
                             | any(.output == \"__i3\")"
}

## TODO: timeout if something goes wrong with the program?
wait_for_window() {
    winid=''
    while [[ -z `xdotool search "$1" "$2" | head -1` ]] ; do
        :
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

CRITERIA="${1:2}=\"$2\""

if [[ -z "$W" ]]; then
    # Launch the program.
    exec_str=( "${@:3}" )
    i3-msg "exec `prepare_exec_str \"${exec_str[@]}\"`" > /dev/null
    # Wait for it to open a window.
    wait_for_window "$option" "$2"
    # Set floating and sticky.
    i3-msg "[$CRITERIA] floating enable, sticky enable" > /dev/null
elif [[ `window_in_scratch "${1:2}" "$2"` == 'true' ]]; then
    # Focus the program.
    i3-msg "[$CRITERIA] move workspace current, move position center" > /dev/null
else
    # Hide the progarm.
    i3-msg "[$CRITERIA] move scratchpad" > /dev/null
fi
