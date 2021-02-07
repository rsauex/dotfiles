#!/usr/bin/env bash

message="$1"

i3-msg -t get_workspaces \
    | jq -r 'sort_by(.focused, .visible != true) | .[].name' \
    | rofi -dmenu -p "${message:?Workspace}"
