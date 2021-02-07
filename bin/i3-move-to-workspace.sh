#!/usr/bin/env bash

new_workspace=`i3-select-workspace.sh 'Move to Workspace'`

if [[ ! -z "$new_workspace" ]]; then
    i3-msg "move workspace ${new_workspace}" > /dev/null
fi
