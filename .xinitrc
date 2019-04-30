# for ssh-agent, see: https://bbs.archlinux.org/viewtopic.php?id=205961
#export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/keyring/ssh"
#gnome-keyring-daemon -rd &

# set the white balance
redshift -O 4800 &

# The the .Xresources, I have more config in there.
xrdb ~/.Xresources &

# real smooth scrolling in FF
export MOZ_USE_XINPUT2=1

# set QT theme engine. requires qt5ct -_-
export QT_QPA_PLATFORMTHEME="qt5ct"

# ibus input stuff
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus 

# XDG_*_DIRs
export XDG_DESKTOP_DIR="$HOME/dev"
export XDG_DOCUMENTS_DIR="$HOME/dev"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_MUSIC_DIR="$HOME/music"
export XDG_VIDEOS_DIR="$HOME/videos"

# start pulseaudio
#start-pulseaudio-x11 &

# start ibus
ibus-daemon -dr &

# Now start i3wm! :)
exec dbus-launch --exit-with-session stumpwm

# Here are some xrandr examples:
#xrandr --output VGA1 --auto --right-of LVDS1
#xrandr --output LVDS1 --auto
#xrandr --output VGA1 --auto --right-of LVDS1
