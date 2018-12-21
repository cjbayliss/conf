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
