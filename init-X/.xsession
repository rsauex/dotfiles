# load xresources
xrdb -merge -I$HOME ~/.Xresources

# disable screen saver
xset s off

# dpms settings
#         standby  suspend  off
xset dpms 0        600      900

# cursor speed
xset r rate 400 44

# lockscreen
xss-lock lock

# keyboard settings - default
setxkbmap -option -option ctrl:nocaps,grp:win_space_toggle,gep_led:scroll -layout us,ua -variant dvp,

# keyboard settings - Ergodox EZ
ergodox_id=$(xinput list --id-only 'ZSA Technology Labs Inc ErgoDox EZ' 2>/dev/null)
[[ -z "$ergodox_id" ]] || setxkbmap -device "$ergodox_id" -option -option -layout us

# DBUS kludge
export `/run/current-system/systemd/bin/systemctl --user show-environment | grep '^DBUS_SESSION_BUS_ADDRESS'`

# make some vars visible in user services
systemctl --user import-environment DISPLAY XAUTHORITY

# start wm
exec i3 &
wmpid=$!

#lxpolkit & # TODO: create a systemd unit

# lockscreen
xss-lock -l -- lock

#~/.fehbg &
#conky &
#emacs --daemon &
#dropbox start &

wait $wmpid
