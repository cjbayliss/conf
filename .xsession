# set the white balance
redshift -O 4800 &

# real smooth scrolling in FF
export MOZ_USE_XINPUT2=1

# enable scolling
#export GDK_CORE_DEVICE_EVENTS=1

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

# Now start stumpwm! :)
exec dbus-launch --exit-with-session stumpwm
