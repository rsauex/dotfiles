#!/bin/bash

current_workspace=`i3-msg -t get_workspaces | jq -r '.[] | select(.focused == true) | .name'`
workenv_dir="${HOME}/work/${current_workspace}"

if [[ -f "$workenv_dir/.envrc" ]]; then
    cd "$workenv_dir"
    . "$workenv_dir/.envrc"
    cd -
fi

exec "$@"
