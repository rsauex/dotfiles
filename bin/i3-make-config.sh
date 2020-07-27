#!/bin/bash

CONFIG_FILE="$XDG_CONFIG_HOME/i3/config"

trap 'rm -f "$TMP_CONFIG_FILE"' EXIT

TEMP_CONFIG_FILE=$(mktemp) || exit 1

for f in $XDG_CONFIG_HOME/i3/[0-9][0-9]*.conf; do
    cat $f >> $TEMP_CONFIG_FILE
done

if ! command -v m4 &> /dev/null; then
    echo 'M4 macro preprocessor is missing!' >&2
    exit 1
fi

M4_VARIABLES="-DPC_TYPE=$(cat /sys/class/dmi/id/chassis_type)"

m4 $M4_VARIABLES $TEMP_CONFIG_FILE > $CONFIG_FILE
