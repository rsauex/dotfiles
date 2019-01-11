#!/bin/sh

# Remove old version
rm -f $XDG_CONFIG_HOME/i3/config
# Create an empty file
touch $XDG_CONFIG_HOME/i3/config

for f in $XDG_CONFIG_HOME/i3/[0-9][0-9]*.conf; do
    cat $f >> $XDG_CONFIG_HOME/i3/config
done
