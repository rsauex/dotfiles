#!/bin/bash

## Get current workspace
current_ws=`i3-msg -t get_workspaces | jq -r '.[] | select(.focused==true).name'`

## Move thunderbird to current workspace and focus it
i3-msg "[class=\"(?i)thunderbird\"] move to workspace ${current_ws}"
i3-msg "[class=\"(?i)thunderbird\"] focus"
