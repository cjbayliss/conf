if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# don't put duplicate lines or lines starting with space in the
# history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# we use -1 to unlimit the history
HISTSIZE=-1

# check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS.
shopt -s checkwinsize

# sane prompt, if not already sane uncomment this
#PS1='[\u@\h \W]\$ '

# alias stuff
alias em='TERM=screen-256color emacs -Q'
alias emacs='TERM=screen-256color emacs'
alias ls='ls -h --color=auto'
alias grep='grep --color=auto'

# cool gcc colors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# cool less colors
export LESS_TERMCAP_mb=$'\E[1;31m'
export LESS_TERMCAP_md=$'\E[1;36m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_us=$'\E[1;32m'
export LESS_TERMCAP_ue=$'\E[0m'
export GROFF_NO_SGR=1
