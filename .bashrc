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

# git info for prompt
__git_branch() {
    BRANCH="$(git branch 2>/dev/null | awk '/^\*/{$1=""; $0=$0; $1=$1; print}')"
    [ -n "$BRANCH" ] && printf "%s" "$BRANCH"
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

# overly complex due to tab completion breaking with simpler approaches -_-
PS1='\[\e[0m\]\u@\h:\[\e[36m\]\w\[\e[0m\]$([ -n "$(__git_branch)" ] && printf " [")\[\e[$(__git_status)m\]$(__git_branch)\[\e[0m\]$([ -n "$(__git_branch)" ] && printf "]")> '

# nice manpage colours
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1m\e[38;5;202m") \
        LESS_TERMCAP_md=$(printf "\e[1m\e[38;5;202m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1m\e[38;5;201m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1m\e[38;5;193m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        man "$@"
}
