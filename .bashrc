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

# see https://dev.to/vlasales/git-prompt-in-bash-47ph
__git_status() {
    STATUS=$(git status 2>/dev/null |
    awk '
    /^On branch / {printf($3)}
    /^You are currently rebasing/ {printf("\\e[38;5;165m rebasing %s", $6)}
    /^Initial commit/ {printf(" (init)")}
    /^Untracked files/ {printf("\\e[38;5;9m +")}
    /^Changes not staged / {printf("\\e[38;5;9m ?")}
    /^Changes to be committed/ {printf("\\e[38;5;9m *")}
    /^Your branch is ahead of/ {printf("\\e[38;5;9m ^")}
    ')
    if [ -n "$STATUS" ]; then
        printf "\e[38;5;193m [$STATUS\e[38;5;193m]"
    fi
}

# cool prompt
PS1='\e[1m\e[38;5;249m\u@\h \e[38;5;202m\W$(__git_status) \e[38;5;15m\$ \e[0m'

###########################
## ALIAS ALL THE THINGS! ##
###########################

# alias stuff
alias ls='ls -h --color=auto'
alias la='ls -hlA --color=auto'
alias ll='ls -hl --color=auto'
alias grep='grep --color=auto'

# instead of exporting TERM=screen-256color which breaks tmux buffers on the
# console, we just alias these to folks.
alias emacs='TERM=screen-256color emacs'
alias mutt='TERM=screen-256color mutt'

######################
## EPIC COMPLETION! ##
######################

bind "set menu-complete-display-prefix on"
bind "set show-all-if-ambiguous on"
bind "TAB:menu-complete"
bind "\"\e[Z\": menu-complete-backward"

#######################
## EXPORTS AND SHEEP ##
#######################

# this is potentionally a bad idea. i do it for proper colours in emacs, however
# it will mess up custom themes in emacs if you use them (i don't). one could
# use an alias for emacs instead, but i've had problems with completion in the
# past as a result.
#
# this is bad because a program will get told it is inside screen, which is not
# ture unless you are inside screen, and could changes it's behaviour based on
# that. (thanks twb on freenode for explaining.)

#export TERM='screen-256color'

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
