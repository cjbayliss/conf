#!/bin/sh

# turn off the screen after 5m
if [ "$(fgconsole 2>/dev/null || echo -1)" -gt 0 ] ; then
    setterm --powersave on --blank 5

    # set redshift
    redshift -m drm -PO 4800 &
fi

# set default umask
umask 077

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
[ -f "$XDG_CONFIG_HOME/sh/shrc" ] && export ENV="$XDG_CONFIG_HOME/sh/shrc"
export EMAIL="snwyi3@protonmail.com"
export NAME="Christopher Bayliss"
export NO_COLOR=1
export MAILCAPS="$MAILCAPS:$XDG_CONFIG_HOME/mutt/mailcap"
export TIME_STYLE=long-iso
export GOPATH="$HOME/.local/go"
export PATH="$PATH:$HOME/.local/bin:$GOPATH/bin"
export LESSHISTFILE='/dev/null'
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export GIT_PAGER="less -F"
export SSH_ASKPASS="emacs-askpass"

# ensure $XDG_*_HOME exists
mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

# start the ssh-agent. requires the package 'keychain'
[ -n "$(which keychain 2>/dev/null)" ] && eval "$(keychain --eval --quiet --quick --timeout 15 --dir $XDG_CACHE_HOME)"

# also start hydroxide
pidof hydroxide >/dev/null || hydroxide serve >/dev/null 2>&1 &
