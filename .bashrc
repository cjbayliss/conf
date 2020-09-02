#!/bin/bash

if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# enable completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# source POSIX ENV
[ -n "$ENV" ] && . "$ENV"

# append to histfile, don't duplicate, unlimit hist size, set histfile
# path
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
mkdir -p "$XDG_DATA_HOME/bash"
HISTFILE="$XDG_DATA_HOME/bash/history"

# check the window size after each command and, if necessary, update the
# values of LINES and COLUMNS.
shopt -s checkwinsize

# better completion (imo)
bind "set colored-completion-prefix on"
bind "set colored-stats on"
bind "set menu-complete-display-prefix on"
bind "set show-all-if-ambiguous on"
bind "set completion-query-items 0"
bind "TAB:menu-complete"
bind "\"\e[Z\": menu-complete-backward"
