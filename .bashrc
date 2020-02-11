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

######################
## GENERAL SETTINGS ##
######################

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# we use -1 to unlimit the history
HISTSIZE=-1
HISTFILE="$HOME/.cache/bash_history"

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# color dirs/files nicely
[ -f /usr/bin/dircolors ] && eval "$(dircolors)"

#################
## COOL PROMPT ##
#################

__git_branch() {
    BRANCH="$(git branch 2>/dev/null | grep '^*' | colrm 1 2)"
    if [ "$BRANCH" != "" ]; then
        printf " %s" "$BRANCH"
    fi
}
__git_status() {
    if [ "$(__git_branch)" != "" ]; then
        if [ "$(git status --short 2>/dev/null)" != "" ]; then
            printf " ✗"
        fi
    fi
}

# universal prompt, should work anywhere
#PS1='\u@\h \w \$ '
# cool prompt with git repo status
PS1='\[\e[1m\]\[\e[38;5;249m\]\u@\h \[\e[38;5;202m\]\w\[\e[38;5;193m\]$(__git_branch)\[\e[31m\]$(__git_status) \[\e[38;5;15m\]\$ \[\e[0m\]'

###########################
## ALIAS ALL THE THINGS! ##
###########################

# alias stuff
alias ls="ls -h --color=auto"
alias grep="grep --color=auto"
alias wget="wget --hsts-file=/dev/null"
# i forgot how slow debians site-lisp is one time and removed this -_-
alias emacs="emacs --no-site-lisp"

# so uh, yeeaaaaah... see https://github.com/tmux/tmux/issues/142
# 😢
alias tmux='tmux -f $HOME/.config/tmux/tmux.conf'

# if on gentoo, make startx use $HOME/.xsession
[ -n "$(uname -r | grep -i gentoo)" ] && alias startx="startx $HOME/.xsession"

######################
## EPIC COMPLETION! ##
######################

bind "set colored-completion-prefix on"
bind "set colored-stats on"
bind "set menu-complete-display-prefix on"
bind "set show-all-if-ambiguous on"
bind "set completion-query-items 0"
bind "TAB:menu-complete"
bind "\"\e[Z\": menu-complete-backward"

#######################
## EXPORTS AND SHEEP ##
#######################

# cool gcc colors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# turn off these history files
export LESSHISTFILE='/dev/null'

# nice man colors
export LESS_TERMCAP_mb=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_md=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1m\e[38;5;201m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1m\e[38;5;193m'
export LESS_TERMCAP_ue=$'\e[0m'
export GROFF_NO_SGR=1

#####################
## CUSTOM COMMANDS ##
#####################

# this provides the main info i used htop for
htop() {
    free -h; echo; uptime; echo; ps au
}

# paste command
ix() {
    curl -F 'f:1=<-' ix.io
}

# double check before powering off
poweroff() {
    echo -n 'ARE YOU SURE!! (y/N)? '
    read answer
    if [ "$answer" != "${answer#[Yy]}" ]; then
        sudo /sbin/poweroff
    fi
}

# if on gentoo, add gentoo-world-check, a @world file checker from:
# https://wiki.gentoo.org/wiki/World_file_(Portage)
# this can be helpfull for removing things from your @world file
if [ -n "$(uname -r | grep -i gentoo)" ]; then

    gentoo-world-check() {
        printf 'Are you sure you want to run gentoo-world-check? IT CAN TAKE SOME TIME!! (y/N)? '
        read -r answer
        if [ "$answer" != "${answer#[Yy]}" ]; then
            rm -i /tmp/deselect
            while read -r i ; do
                if [ -n "$(qdepends -Q $i)" ]; then
                    printf "\nchecking %s\n" "$i"
                    if [ -n "$(emerge -p --quiet --depclean $i)" ]; then
                        echo "$i needs to stay in @world"
                    else
                        echo "$i can be deselected"
                        echo "$i" >> /tmp/deselect
                    fi
                fi
            done < /var/lib/portage/world
            printf "\n/tmp/deselect contains candidates for deselection\n"
            printf "try: emerge --ask --deselect \$(< /tmp/deselect)\n"
        fi
    }

fi

# cleanup on exit
__cleanup() {
    # don't run unless the login shell
    if [[ $- == *i* ]]; then
        rmdir ~/*
        rm ~/.*hist*
        rm ~/.lesshst
        rm ~/.viminfo
        rm -rf ~/.w3m
        clear
    fi
}

trap __cleanup EXIT
