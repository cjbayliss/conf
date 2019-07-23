# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]] ; then
    . ~/.bashrc
fi

# change email addr and uncomment
#export EMAIL="email@example.com"
export NAME="Christopher Bayliss"

# add ~/.bin to PATH
export PATH="$PATH:$HOME/.bin"

# make virsh use qemu:///system by default
export LIBVIRT_DEFAULT_URI='qemu:///system'

# start the ssh-agent. requires the package 'keychain'
eval $(keychain --eval --quiet --quick)

# set default directories
xdg-user-dirs-update --set DESKTOP "$HOME/dev"
xdg-user-dirs-update --set DOWNLOAD "$HOME/downloads"
xdg-user-dirs-update --set TEMPLATES "$HOME/dev/templates"
xdg-user-dirs-update --set PUBLICSHARE "$HOME/dev/public"
xdg-user-dirs-update --set DOCUMENTS "$HOME/dev"
xdg-user-dirs-update --set MUSIC "$HOME/music"
xdg-user-dirs-update --set PICTURES "$HOME/pictures"
xdg-user-dirs-update --set VIDEOS "$HOME/videos"
