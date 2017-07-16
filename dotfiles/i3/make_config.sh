#!/bin/sh
cat $HOME/.i3/config.base \
    $HOME/.i3/config.menus \
    $HOME/.i3/config.keys \
    $HOME/.i3/config.keys.wm \
    $HOME/.i3/config.bar > $HOME/.i3/config
