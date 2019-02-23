# My standard GNU/Linux config

Some of it is nice, but most of it is boring.

See [emacs-config](https://git.sr.ht/~cjb/emacs-config) for my emacs config.

## mutt shortcuts for the forgetful, aka cjb.

To tag emails, press `t`.

You can then 'move' (know as 'save' in mutt) them with `;s` and type
the folder you wish to move to, if it doesn't exist mutt will ask if
you want to create it.

Note that ';' is used to apply next function to tagged messages.

To switch folder, press `c-?`.

To purge deleted messages, press `$`.

To mark for deletion, press `d`.

To unmark for deletion, press `u`.

## ffmpeg

convert music to opus:

```
for i in *.mp3; do ffmpeg -i "$i" -acodec libopus -b:a 256000 -vbr off "${i%.*}.opus"; done
```

cut out section:

```
ffmpeg -i in.opus -ss 00:00:20 -to 00:00:40 -c copy out.opus
```

## input emoji and japanese

If you are in emacs toggle the input method with <kbd>C-\\</kbd>, and
<kbd>C-x RET C-\\</kbd> to select a different input.

Install `ibus`, `ibus-mozc`, and on debian `ibus-table-emoji`.

Restart the session, and the config will be loaded.

Run `ibus-setup` to configure `ibus` and add the `ibus-mozc`
input. `~/.bin/ibus-toggle` is bound to
<kbd>Cmd</kbd>+<kbd>Space</kbd> in the sway config. Set the emoji
input to something like <kbd>Cmd</kbd>+<kbd>e</kbd> and the unicode
input to something like <kbd>Cmd</kbd>+<kbd>u</kbd>.

### Set Hiragana shortcut

Open the mozc preferences and find 'Keymap style', press
Customize. Click Edit > New entry. Set the mode to 'Direct input', the
key to <kbd>f4</kbd>, and the command to 'Set input mode to
Hiragana'. You will need to restart everything, then when you next
switch to mozc japanese input, press <kbd>f4</kbd> and you can start
typing Hiragana right away. You only need to press <kbd>f4</kbd> once
per session.
