#!/bin/sh

# configure XDG_*_{DIR,HOME} stuff
if [ -z "${XDG_RUNTIME_DIR}" ]; then
    export XDG_RUNTIME_DIR="/tmp/${UID:=$(id -u "$USER")}-runtime-dir"
    if [ ! -d "${XDG_RUNTIME_DIR}" ]; then
        mkdir "${XDG_RUNTIME_DIR}"
        chmod 0700 "${XDG_RUNTIME_DIR}"
    fi
fi
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$XDG_RUNTIME_DIR/cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# ensure $XDG_*_HOME exists
mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

# OS specific stuff
if [ "$(uname -s)" = "Darwin" ]; then
    [ -f "$XDG_CONFIG_HOME/sh/os/macos" ] && . "$XDG_CONFIG_HOME/sh/os/macos"
elif [ "$(uname -s)" = "Linux" ]; then
    [ -f "$XDG_CONFIG_HOME/sh/os/linux" ] && . "$XDG_CONFIG_HOME/sh/os/linux"
fi

# tool specific stuff
for __tool in "$XDG_CONFIG_HOME"/sh/tools/*; do
    [ -f "$__tool" ] && . "$__tool"
done
unset __tool

export LESSHISTFILE='/dev/null'
export MAILCAPS="$MAILCAPS:$XDG_CONFIG_HOME/mutt/mailcap"
export PAGER=cat
export TIME_STYLE=long-iso

export NAME='Christopher Bayliss'
export EMAIL='cjbdev@icloud.com'

# some programs respect this
export DISABLE_TELEMETRY=1

# finally set $PATH
mkdir -p "$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/bin"

# generic shell config
[ -f "$XDG_CONFIG_HOME/sh/shrc" ] && export ENV="$XDG_CONFIG_HOME/sh/shrc"

# bash/zsh aren't POSIX compliant despite claims otherwise...
[ -n "$BASH_VERSION" ] || [ -n "$ZSH_VERSION" ] && [ -f "$XDG_CONFIG_HOME/sh/shrc" ] && . "$XDG_CONFIG_HOME/sh/shrc"
