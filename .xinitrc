# This deals with dbus, etc.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# My ssh agent
eval "$($HOME/.bin/twb-agents)"

# set the white balance
redshift -O 5200 &

# The the .Xresources, I have more config in there.
xrdb ~/.Xresources

# start pulseaudio
start-pulseaudio-x11

# Now start i3wm! :)
dbus-launch --exit-with-session i3

# Here are some xrandr examples:
#xrandr --output VGA1 --auto --right-of LVDS1
#xrandr --output LVDS1 --auto
#xrandr --output VGA1 --auto --right-of LVDS1
