#!/bin/bash

if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# enable bash completion in interactive shells
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# source POSIX ENV
[ -n "$ENV" ] && . "$ENV"

# append to histfile, don't duplicate, unlimit hist size, set histfile path
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
HISTFILE="$XDG_CACHE_HOME/bash_history"

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# better completion (imo)
bind "set colored-completion-prefix on"
bind "set colored-stats on"
bind "set menu-complete-display-prefix on"
bind "set show-all-if-ambiguous on"
bind "set completion-query-items 0"
bind "TAB:menu-complete"
bind "\"\e[Z\": menu-complete-backward"

# set cool prompt
__git_branch() {
    BRANCH="$(git branch 2>/dev/null | grep '^*' | colrm 1 2)"
    if [ -n "$BRANCH" ]; then
        printf "%s" "$BRANCH"
    fi
}

__git_status() {
    if [ -n "$(__git_branch)" ]; then
        if [ -n "$(git status --short 2>/dev/null)" ]; then
            printf "1;31"
        else
            printf "32"
        fi
    fi
}

# the complexity is due to tab completion breaking with simpler approaches
PS1='\[\e[0m\]\u@\h:\[\e[36m\]\w\[\e[0m\]$([ -n "$(__git_branch)" ] && printf " [")\[\e[$(__git_status)m\]$(__git_branch)\[\e[0m\]$([ -n "$(__git_branch)" ] && printf "]")> '

# nice man colors
export LESS_TERMCAP_mb=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_md=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1m\e[38;5;201m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1m\e[38;5;193m'
export LESS_TERMCAP_ue=$'\e[0m'
export GROFF_NO_SGR=1
