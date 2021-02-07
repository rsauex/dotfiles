#!/usr/bin/env bash

## <message> <program>

MESSAGE=$1; shift
PROGRAM=$1; shift
ARGS="$@"

i3-msg -t get_tree | jq -r "recurse(.nodes[], .floating_nodes[]) | select (has(\"window_properties\")) | select (.window_properties.class | test(\"^alacritty$\"; \"i\")) | .name" \
    | rofi -dmenu -p "$MESSAGE"
