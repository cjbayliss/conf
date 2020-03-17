# My standard GNU/Linux config

Some of it is nice, but most of it is boring.

In 2019 I re-created this repo, deleting all the old history as it was mostly
useless to me. This makes cloning quicker (was already fast). Obviously I kept a
backup, if you need something that you think was in an old commit, email me. :)

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

Install `ibus`, `ibus-anthy`, and on debian `ibus-table-emoji`.

Restart the session, and the config will be loaded.

Run `ibus-setup` to configure `ibus` and add the `anthy`
input. `~/.bin/ibus-toggle` is bound to
<kbd>Cmd</kbd>+<kbd>Space</kbd> in the sway config. Set the emoji
input to something like <kbd>Cmd</kbd>+<kbd>e</kbd> and the unicode
input to something like <kbd>Cmd</kbd>+<kbd>u</kbd>.
