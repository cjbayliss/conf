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

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

#################
## COOL PROMPT ##
#################

__git_branch() {
    BRANCH="$(git branch 2>/dev/null | grep '^*' | colrm 1 2)"
    if [ "$BRANCH" != "" ]; then
        printf " $BRANCH"
    fi
}
__git_status() {
    if [ "$(__git_branch)" != "" ]; then
        if [ "$(git status --short 2>/dev/null)" != "" ]; then
            printf " ✗"
        fi
    fi
}

# cool prompt
#PS1='\[\e[1m\]\u@\h \[\e[33m\]\W\[\e[35m\]$(__git_branch)\[\e[31m\]$(__git_status) \[\e[39m\]\$ \[\e[0m\]'
PS1='\[\e[1m\]\[\e[38;5;249m\]\u@\h \[\e[38;5;202m\]\W\[\e[38;5;193m\]$(__git_branch)\[\e[31m\]$(__git_status) \[\e[38;5;15m\]\$ \[\e[0m\]'

###########################
## ALIAS ALL THE THINGS! ##
###########################

# alias stuff
alias ls="ls -h --color=auto"
alias la="ls -hlA"
alias ll="ls -hl"
alias grep="grep --color=auto"
alias sbcl="rlwrap sbcl --noinform"

# the holy grail of editors!! READLINE CAT. poof!! your mind is blown, innit??!!
alias cat="rlwrap cat"

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

# cool gcc colors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# cool less colors. this is buggy.
export LESS_TERMCAP_mb=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_md=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1m\e[38;5;201m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1m\e[38;5;193m'
export LESS_TERMCAP_ue=$'\e[0m'
export GROFF_NO_SGR=1
