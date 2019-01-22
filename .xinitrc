# for ssh-agent, see: https://bbs.archlinux.org/viewtopic.php?id=205961
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/keyring/ssh"
gnome-keyring-daemon -rd &

# set the white balance
redshift -O 4800 &

# The the .Xresources, I have more config in there.
xrdb ~/.Xresources &

# real smooth scrolling in FF
export MOZ_USE_XINPUT2=1

# ibus input stuff
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus 

# start pulseaudio
start-pulseaudio-x11 &

# Now start i3wm! :)
exec dbus-launch --exit-with-session i3

# Here are some xrandr examples:
#xrandr --output VGA1 --auto --right-of LVDS1
#xrandr --output LVDS1 --auto
#xrandr --output VGA1 --auto --right-of LVDS1
