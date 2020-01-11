# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]] ; then
    . ~/.bashrc
fi

# turn off the screen after 1 min
if [[ "$(fgconsole 2>/dev/null || echo -1)" -gt 0 ]] ; then
    setterm --powersave on --blank 1
fi

# change email addr and uncomment
#export EMAIL="email@example.com"
export NAME="Christopher Bayliss"

# add ~/.bin to PATH
export PATH="$PATH:$HOME/.bin:$HOME/.local/bin"

# make virsh use qemu:///system by default
export LIBVIRT_DEFAULT_URI='qemu:///system'

# start the ssh-agent. requires the package 'keychain'
eval $(keychain --eval --quiet --quick)
