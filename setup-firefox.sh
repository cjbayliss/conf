#!/bin/sh

# Make sure firefox is closed, and delete ~/.mozilla before running this
# script. Only tested on gentoo, which has the main firefox directory
# in: /usr/lib64/firefox/

# first delete distro's ini, this is noramlly used to setup bookmarks 🙄
[ -f /usr/lib64/firefox/distribution/distribution.ini ] && rm /usr/lib64/firefox/distribution/distribution.ini

# now copy custom firefox settings to system
if [ -d /usr/lib64/firefox ]; then
    cp ./usr/lib64/firefox/browser/defaults/preferences/cjb.js /usr/lib64/firefox/browser/defaults/preferences/
    cp ./usr/lib64/firefox/distribution/policies.json /usr/lib64/firefox/distribution/
fi
