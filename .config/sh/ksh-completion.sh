# FIXME:: get emerge completions can not slow down new ksh shells

# basic git completion
[ -f /usr/bin/git ] && set -A complete_git_1 -- add bisect branch \
                           checkout clone commit diff fetch grep \
                           init log merge mv pull push rebase reset \
                           restore rm show status switch tag

# i use these options a lot in mpv
[ -f /usr/bin/mpv ] && set -A complete_mpv_1 -- '-vo' '--aid'

# basic man page suggestion NOTE: only .1 pages
set -A complete_man_1 -- $(ls -1 /usr/share/man/man1/ | sed 's/\..*//')

# tab complete some commands for sudo
[ -f /usr/bin/emerge ] && GENTOO_STUFF='emerge eselect'
set -A complete_sudo_1 -- $GENTOO_STUFF emacs poweroff -i
