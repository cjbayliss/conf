xrandr --output VGA1 --auto --right-of LVDS1
xrandr --output LVDS1 --auto
xrandr --output VGA1 --auto --right-of LVDS1
eval "$($HOME/.bin/twb-agents)"
xrdb .Xdefaults
exec i3
#startxfce4
#exec gnome-session
