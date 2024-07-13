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
fi
export GROFF_NO_SGR=1 # required for man colours to work

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

export GDK_DPI_SCALE=0.5
export XCURSOR_SIZE=24
export XCURSOR_THEME=Adwaita

# some programs respect this
export DISABLE_TELEMETRY=1

# finally set $PATH
export PATH="$PATH:$HOME/.local/bin"

# ensure $XDG_*_HOME exists
mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

[ -n "$BASH_VERSION" ] && [ -f "$XDG_CONFIG_HOME/sh/shrc" ] && . "$XDG_CONFIG_HOME/sh/shrc"
[ -f "$XDG_CONFIG_HOME/dircolors" ] && eval "$(dircolors -b $XDG_CONFIG_HOME/dircolors)"

# start the ssh-agent.
pidof -q ssh-agent || eval "$(ssh-agent)" >/dev/null
