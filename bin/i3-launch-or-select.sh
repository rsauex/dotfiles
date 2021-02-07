#!/usr/bin/env bash

## <message> <program> <arg>...

MESSAGE="$1"; shift
PROGRAM="$1"; shift
ARGS="$@"

window_name=`i3-select-program.sh "$MESSAGE" "$PROGRAM"`
escaped_window_name=$(echo "$window_name" | sed 's/\\/\\\\/g' | sed 's/\"/\\\"/g')

if [[ ! -z "$window_name" ]]; then
    window_id=`i3-msg -t get_tree | jq -r "[recurse(.nodes[], .floating_nodes[]) | select(.name == \"$escaped_window_name\") | .id] | first"`
    if [[ "$window_id" == "null" ]]; then
        $PROGRAM "${ARGS[@]}" "$window_name"
    else
        i3-msg "[con_id=${window_id}] focus" > /dev/null
    fi
fi
