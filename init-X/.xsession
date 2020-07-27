# load xresources
xrdb -merge -I$HOME ~/.Xresources

# screen blanking time
xset s off
xset dpms 0 600 900

# cursor speed
xset r rate 400 44

# keyboard settings
if [[ xinput | grep "Ergodox" &>/dev/null ]]; then
    # Ergodox EZ is connected - use QWERY
    setxkbmap -option -option ctrl:nocaps,grp:win_space_toggle,gep_led:scroll -layout us,ua
else
    # Ergodox EZ is not connected - use DVORAK
    setxkbmap -option -option ctrl:nocaps,grp:win_space_toggle,gep_led:scroll -layout us,ua -variant dvp,
fi

exec i3 &
wmpid=$!

systemctl --user import-environment
systemctl --user start x-session.target

lxpolkit & # TODO: create a systemd unit

#~/.fehbg &
#conky &
#emacs --daemon &
#dropbox start &

wait $wmpid
