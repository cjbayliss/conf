# My standard GNU/Linux config

Some of it is nice, but most of it is boring.

See [emacs-config](https://git.sr.ht/~cjb/emacs-config) for my emacs config.

## mutt shortcuts for the forgetful, aka cjb.

To tag emails, press `t`.

You can then 'move' (know as 'save' in mutt) them with `s` and type
the folder you wish to move to, if it doesn't exist mutt will ask if
you want to create it.

To switch folder, press `c-?`.

To purge deleted messages, press `$`.

To mark for deletion, press `d`.

To unmark for deletion, press `u`.

## converting music to opus

```
for i in *.mp3; do ffmpeg -i "$i" -acodec libopus -b:a 256000 -vbr off "${i%.*}.opus"; done
```

## input emoji and japanese

If you are in emacs toggle the input method with `C-\`, and `C-x RET
C-\` to switch to a different input.

Install `ibus`, `ibus-mozc`, and on debian `ibus-table-emoji`.

Restart the session, and the config will be loaded.

Configure `ibus` and add the `ibus-mozc` input. You will possibly want
to set the input switching to Super+Space or something. Also set the
emoji input to something like Super+E and the unicode input to
something like Super+U.

Also, right click the ibus icon in i3status bar > Preferences > Input
Method > Japanese - mozc > Preferences. Find 'Keymap style', and
press Customize. Click Edit > New entry. Set the mode to 'Direct
input', the key to 'f4', and the command to 'Set input mode to
Hiragana'. You will need to restart everything, then when you next
switch to mozc japanese input, press `f4` and you can start typing
Hiragana right away. You only need to press `f4` once per session.
