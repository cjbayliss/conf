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
alias ls='ls -h --color=auto'
alias grep='grep --color=auto'

#######################
## exports and sheep ##
#######################

# this is potentionally a bad idea. i do it for proper colours in
# emacs, however it will mess up custom themes in emacs if you use
# them (i don't). one could use an alias for emacs instead, but i've
# had problems with completion in the past as a result.
#
# this is bad because a program will get told it is inside screen,
# which is not ture unless you are inside screen, and could changes
# it's behaviour based on that. (thanks twb on freenode for
# explaining.)
export TERM='screen-256color'

# cool gcc colors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# cool less colors. this is buggy.
export LESS_TERMCAP_mb=$'\E[1;31m'
export LESS_TERMCAP_md=$'\E[1;33m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;33m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_us=$'\E[1;36m'
export LESS_TERMCAP_ue=$'\E[0m'
export GROFF_NO_SGR=1
