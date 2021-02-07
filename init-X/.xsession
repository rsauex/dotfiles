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
ergodox_id=$(xinput list --id-only '???' 2>/dev/null)
[[ -z "$ergodox_id" ]] || setxkbmap -device "$ergodox_id" -option -layout us

# start wm
exec i3 &
wmpid=$!

systemctl --user import-environment
systemctl --user start x-session.target

lxpolkit & # TODO: create a systemd unit

# lockscreen
xss-lock lock

#~/.fehbg &
#conky &
#emacs --daemon &
#dropbox start &

wait $wmpid
