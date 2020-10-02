#!/bin/sh

# turn off the screen after 1 min
if [ "$(fgconsole 2>/dev/null || echo -1)" -gt 0 ] ; then
    setterm --powersave on --blank 1
fi

# 🐑
if [ -z "${XDG_RUNTIME_DIR}" ]; then
    export XDG_RUNTIME_DIR=/tmp/${UID:=$(id -u $USER)}-runtime-dir
    if [ ! -d "${XDG_RUNTIME_DIR}" ]; then
        mkdir "${XDG_RUNTIME_DIR}"
        chmod 0700 "${XDG_RUNTIME_DIR}"
    fi
fi
export XDG_CONFIG_HOME="$HOME/.config"
# why store this? put it in /tmp
export XDG_CACHE_HOME="$XDG_RUNTIME_DIR/cache"
export XDG_DATA_HOME="$HOME/.local/share"
export ENV="$XDG_CONFIG_HOME/sh/shrc"
export EMAIL="christopher.j.bayliss@gmail.com"
export NAME="Christopher Bayliss"
export NO_COLOR=1
export TIME_STYLE=long-iso
export PATH="$PATH:$HOME/.local/bin"
export LESSHISTFILE='/dev/null'
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export GIT_PAGER="less -F"

# find a suitable editor
if [ -f "$(which ed)" ]; then
    EDITOR="ed"
elif [ -f "$(which vi)" ]; then
    EDITOR="vi"
elif [ -f "$(which mg)" ]; then
    EDITOR="mg"
elif [ -f "$(which nano)" ]; then
    EDITOR="nano"
else
    echo "WHAA! does this system even have an editor!?"
fi
export EDITOR
export VISUAL="$EDITOR"

# ensure $XDG_*_HOME exists
mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

# start the ssh-agent. requires the package 'keychain'
[ -f /usr/bin/keychain ] && eval "$(keychain --eval --quiet --quick --dir $XDG_CACHE_HOME)"

# set default directories
if [ -f /usr/bin/xdg-user-dirs-update ]; then
    xdg-user-dirs-update --set DESKTOP "$HOME/dev/desktop"
    xdg-user-dirs-update --set DOWNLOAD "$HOME/downloads"
    xdg-user-dirs-update --set TEMPLATES "$HOME/dev/templates"
    xdg-user-dirs-update --set PUBLICSHARE "$HOME/dev/public"
    xdg-user-dirs-update --set DOCUMENTS "$HOME/dev"
    xdg-user-dirs-update --set MUSIC "$HOME/music"
    xdg-user-dirs-update --set PICTURES "$HOME/pictures"
    xdg-user-dirs-update --set VIDEOS "$HOME/videos"
fi
