#!/usr/bin/env bash

new_workspace=`i3-select-workspace.sh 'Workspace'`

if [[ ! -z "$new_workspace" ]]; then
    i3-msg "workspace ${new_workspace}" > /dev/null
fi
