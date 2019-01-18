#!/bin/sh

# Remove old version
rm -f $HOME/.i3/config
# Create an empty file
touch $HOME/.i3/config

for f in $HOME/.i3/[0-9][0-9]*.conf; do
    cat $f >> $HOME/.i3/config
done
