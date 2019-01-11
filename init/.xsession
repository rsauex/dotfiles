# load xresources
xrdb -merge -I$HOME ~/.Xresources

# screen blanking time
xset s off
xset dpms 0 600 900

# cursor speed
xset r rate 400 44

# keyboard settings
setxkbmap -layout us,ua -variant dvp, -option ctrl:nocaps,grp:lctrl_toggle,gep_led:scroll

exec i3 &
wmpid=$!

~/.fehbg &
conky &
nm-applet &
blueman-applet &
compton -b &
emacs --daemon &
#gajim &
dropbox start &
wait $wmpid
