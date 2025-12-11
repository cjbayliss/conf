#!/bin/sh

# turn off the screen after 5m
if [ "$(fgconsole 2>/dev/null || echo -1)" -gt 0 ]; then
    setterm --powersave on --blank 5
fi

# load system environment variables first
[ -f /etc/profile.env ] && . /etc/profile.env

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
export XDG_DATA_DIRS="$XDG_DATA_DIRS:/home/cjb/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share"
[ -f "$XDG_CONFIG_HOME/sh/shrc" ] && export ENV="$XDG_CONFIG_HOME/sh/shrc"

export EDITOR="kak"
export VISUAL="$EDITOR"

export LESSHISTFILE='/dev/null'
export MAILCAPS="$MAILCAPS:$XDG_CONFIG_HOME/mutt/mailcap"
export MANPAGER='less --mouse --wheel-lines 3'
export MANWIDTH=72
export PAGER=cat
export TIME_STYLE=long-iso

if [ -z "$SSH_CONNECTION" ]; then
    # don't run tput on a potentially unkown terminal
    __RESET_COLORS="$(tput sgr0)"
    __BOLD="$(tput bold)"
    __RED="$(tput setaf 1)"
    __BRIGHT_BLUE="$(tput setaf 12)"
    __BRIGHT_CYAN="$(tput setaf 14)"

    # man colours
    export LESS_TERMCAP_mb="$__BOLD$__RED"
    export LESS_TERMCAP_md="$__BOLD$__RED"
    export LESS_TERMCAP_me="$__RESET_COLORS"
    export LESS_TERMCAP_so="$__BOLD$__BRIGHT_BLUE"
    export LESS_TERMCAP_se="$__RESET_COLORS"
    export LESS_TERMCAP_us="$__BOLD$__BRIGHT_CYAN"
    export LESS_TERMCAP_ue="$__RESET_COLORS"
    export GROFF_NO_SGR=1 # required for man colours to work
fi

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export NAME='Christopher Bayliss'
export EMAIL='cjbdev@icloud.com'

export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPATH="$XDG_DATA_HOME/go"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NPM_CONFIG_TMP="$XDG_RUNTIME_DIR/npm"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
[ -f "$XDG_CONFIG_HOME/python/startup.py" ] && export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/startup.py"

export SCREENRC="$XDG_CONFIG_HOME/screen/screenrc"

# some programs respect this
export DISABLE_TELEMETRY=1

# finally set $PATH
mkdir -p "$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/bin"

# IMPORTANT: NEVER!!! use xdg-user-dirs-update, it is broken as fuck
export XDG_DESKTOP_DIR="$HOME/stuff/desktop"
export XDG_DOCUMENTS_DIR="$HOME/stuff"
export XDG_DOWNLOAD_DIR="$HOME/stuff/downloads"
export XDG_MUSIC_DIR="$HOME/music"
export XDG_PICTURES_DIR="$HOME/pictures"
export XDG_PUBLICSHARE_DIR="$HOME/stuff/public"
export XDG_TEMPLATES_DIR="$HOME/stuff/templates"
export XDG_VIDEOS_DIR="$HOME/videos"

# ensure $XDG_*_HOME exists
mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"
mkdir -p "$XDG_DESKTOP_DIR" "$XDG_DOCUMENTS_DIR" "$XDG_DOWNLOAD_DIR" "$XDG_MUSIC_DIR" "$XDG_PICTURES_DIR" "$XDG_PUBLICSHARE_DIR" "$XDG_TEMPLATES_DIR" "$XDG_VIDEOS_DIR"

[ -n "$BASH_VERSION" ] && [ -f "$XDG_CONFIG_HOME/sh/shrc" ] && . "$XDG_CONFIG_HOME/sh/shrc"
[ -f "$XDG_CONFIG_HOME/dircolors" ] && eval "$(dircolors -b $XDG_CONFIG_HOME/dircolors)"

# start the ssh-agent.
if [ -z "${WAYLAND_DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
    pkill -9 ssh-agent # FIXME: OpenRC user services "starts" ssh-agent, very incorrectly
    pidof -q ssh-agent || eval "$(ssh-agent)" >/dev/null
fi
