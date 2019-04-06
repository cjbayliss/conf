# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]] ; then
	. ~/.bashrc
fi

# add ~/.bin to PATH
export GOPATH="$HOME/.go"
export PATH="$PATH:$HOME/.bin:$GOPATH/bin"

# start the ssh-agent. requires the package 'keychain'
eval $(keychain --eval --quiet --quick)
