# load xresources
xrdb -merge -I$HOME ~/.Xresources

# screen blanking time
xset s off
xset dpms 0 600 900

# cursor speed
xset r rate 400 44

# keyboard settings
setxkbmap -option -layout us,ua -variant dvp, -option ctrl:nocaps,grp:win_space_toggle,gep_led:scroll

exec i3 &
wmpid=$!

systemctl --user import-environment
systemctl --user start x-session.target

#~/.fehbg &
#conky &
#blueman-applet &
#emacs --daemon &
#dropbox start &

wait $wmpid
