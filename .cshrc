set prompt = "[`whoami`@`hostname -s` `basename "$PWD"`]$ "

set history = 1000
set savehist = (1000 merge)
set histfile = ~/.tcsh_history

setenv EDITOR /usr/bin/emacs

alias ls 'ls -h --color'
alias grep 'grep --color=auto'
