#!/bin/sh

# turn off the screen after 1 min
if [ "$(fgconsole 2>/dev/null || echo -1)" -gt 0 ] ; then
    setterm --powersave on --blank 1
fi

# 🐑
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

if test -z "${XDG_RUNTIME_DIR}"; then
    export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir
    if ! test -d "${XDG_RUNTIME_DIR}"; then
        mkdir "${XDG_RUNTIME_DIR}"
        chmod 0700 "${XDG_RUNTIME_DIR}"
    fi
fi

export ENV="$XDG_CONFIG_HOME/sh/shrc"
export EMAIL="cjb@cjb.sh"
export NAME="Christopher Bayliss"
export TIME_STYLE=long-iso
export PATH="$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.config/composer/vendor/bin:$HOME/.local/share/npm/bin"
export LESSHISTFILE='/dev/null'
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export GIT_PAGER="diff-highlight | less -F -X"
export MOZC_CONFIGURATION_DIRECTORY="$XDG_CONFIG_HOME/mozc"
# should i be compelled to use npm, set some sane stuff
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NPM_CONFIG_TMP="$XDG_RUNTIME_DIR/npm"

# make virsh use qemu:///system by default
export LIBVIRT_DEFAULT_URI='qemu:///system'

# start the ssh-agent. requires the package 'keychain'
[ -f /usr/bin/keychain ] && eval "$(keychain --eval --quiet --quick --dir ~/.cache/keychain)"

# color dirs/files nicely
[ -f /usr/bin/dircolors ] && eval "$(dircolors)"

# set default directories
if [ -f /usr/bin/xdg-user-dirs-update ]; then
    xdg-user-dirs-update --set DESKTOP "$HOME/dev"
    xdg-user-dirs-update --set DOWNLOAD "$HOME/downloads"
    xdg-user-dirs-update --set TEMPLATES "$HOME/dev/templates"
    xdg-user-dirs-update --set PUBLICSHARE "$HOME/dev/public"
    xdg-user-dirs-update --set DOCUMENTS "$HOME/dev"
    xdg-user-dirs-update --set MUSIC "$HOME/music"
    xdg-user-dirs-update --set PICTURES "$HOME/pictures"
    xdg-user-dirs-update --set VIDEOS "$HOME/videos"
fi

# NOTE: this *must* go last
if [ -n "$BASH_VERSION" ] && [ -r ~/.bashrc ] ; then
    . ~/.bashrc
fi
