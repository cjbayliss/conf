# This deals with dbus, etc.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# My ssh agent
eval "$($HOME/.bin/twb-agents)"

# The the .Xresources, I have more config in there.
xrdb ~/.Xresources

# HP 23fi or 23cw colour correction
#xgamma -bgamma 1.2 -ggamma 1.1 -rgamma 1.1

# wallpaper. requires the background to be set with feh, ex:
# feh --bg-scale path/to/pic.jpg
#sh ~/.fehbg &

# for pulseaudio systems
#start-pulseaudio-x11

# Now start i3wm! :)
exec i3

# Here are some xrandr examples:
#xrandr --output VGA1 --auto --right-of LVDS1
#xrandr --output LVDS1 --auto
#xrandr --output VGA1 --auto --right-of LVDS1
