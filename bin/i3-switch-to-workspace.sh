#!/bin/bash

new_workspace=`i3-msg -t get_workspaces | jq -r '.[].name' | rofi -dmenu -p 'Workspace'`

if [[ ! -z "$new_workspace" ]]; then
    i3-msg "workspace ${new_workspace}" > /dev/null
fi
