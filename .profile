# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]] ; then
    . ~/.bashrc
fi

# change email addr and uncomment
#export EMAIL="email@example.com"
export NAME="Christopher Bayliss"

# add ~/.bin to PATH
export PATH="$PATH:$HOME/.bin"

# start the ssh-agent. requires the package 'keychain'
eval $(keychain --eval --quiet --quick)
