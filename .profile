#!/bin/sh

# turn off the screen after 5m
if [ "$(fgconsole 2>/dev/null || echo -1)" -gt 0 ] ; then
    setterm --powersave on --blank 5

    # set redshift
    [ -n "$(command -v redshift 2>/dev/null)" ] && redshift -m drm -PO 4800 &
fi

# set default umask
umask 077

# 🐑
if [ -z "${XDG_RUNTIME_DIR}" ]; then
    export XDG_RUNTIME_DIR="/tmp/${UID:=$(id -u "$USER")}-runtime-dir"
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

export EDITOR="hx"
export VISUAL="$EDITOR"

export LESSHISTFILE='/dev/null'
export MAILCAPS="$MAILCAPS:$XDG_CONFIG_HOME/mutt/mailcap"
export MANPAGER='less --mouse --wheel-lines 3'
export MANWIDTH=72
export PAGER=cat
export GIT_PAGER="$PAGER"
export PATH="$PATH:$HOME/.local/bin"
export TIME_STYLE=long-iso

# man colours
export LESS_TERMCAP_mb="$(tput bold; tput setaf 1)"
export LESS_TERMCAP_md="$(tput bold; tput setaf 1)"
export LESS_TERMCAP_me="$(tput sgr0)"
export LESS_TERMCAP_so="$(tput bold; tput setaf 12)"
export LESS_TERMCAP_se="$(tput sgr0)"
export LESS_TERMCAP_us="$(tput bold; tput setaf 14)"
export LESS_TERMCAP_ue="$(tput sgr0)"
# required for man colours to work
export GROFF_NO_SGR=1

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export MOZ_GTK_TITLEBAR_DECORATION=system
export MOZ_USE_XINPUT2=1

export NAME='Christopher Bayliss'
export EMAIL='cjbdev@icloud.com'

export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NPM_CONFIG_TMP="$XDG_RUNTIME_DIR/npm"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/startup.py"

export GDK_DPI_SCALE=0.5
export XCURSOR_SIZE=24
export XCURSOR_THEME=Adwaita

# ensure $XDG_*_HOME exists
mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

[ -n "$BASH_VERSION" ] && [ -f "$XDG_CONFIG_HOME/sh/shrc" ] && . "$XDG_CONFIG_HOME/sh/shrc"

# start the ssh-agent. requires the package 'keychain'
[ -n "$(command -v keychain 2>/dev/null)" ] && eval "$(keychain --eval --quiet --quick --timeout 15 --dir "$XDG_CACHE_HOME")"
