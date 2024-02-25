#!/usr/bin/env bash

current_workspace=`i3-msg -t get_workspaces | jq -r '.[] | select(.focused == true) | .name'`
if [[ "work" == "${current_workspace}" ]]; then
    project_dir="${HOME}/work"
else
    project_dir="${HOME}/Projects/${current_workspace}"
fi

if [[ -d "${project_dir}" ]]; then
    export PROJECT_HOME="${project_dir}"
    cd "${PROJECT_HOME}"

    if [[ -d "${PROJECT_HOME}/.profile/profile" ]]; then
        export GUIX_PROFILE="${PROJECT_HOME}/.profile/profile"

        # Load Guix Profile if it exists
        if [[ -f "${GUIX_PROFILE}/etc/profile" ]]; then
            . "${GUIX_PROFILE}/etc/profile"
        fi

        # Load Project Profile if it exists
        if [[ -f "${GUIX_PROFILE}/etc/project-profile" ]]; then
            . "${GUIX_PROFILE}/etc/project-profile"
        fi
    fi
fi

exec "$@"
