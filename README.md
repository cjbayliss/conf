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

Install `fcitx-frontend-all`, `fcitx-mozc`, and `fcitx-table-emoji` or
`fcitx-table-other`.

Restart the session, and the config in `.xsession` will be loaded.

Configure `fcitx` and add the `mozc` input. Then go to the addon tab
and configure the 'Input method selector' and set 'Global Input Method
SelectKey' to Alt+Super+Space or something. And こんにちわ！ (only
word I can really remeber so far, that and ぎんたま。I guess there is
also あんき。)

To input emoji press `Ctl+Atl+Shift+U` and type something like
'smiling face' you can then look through a list of smiling emoji. You
can use this to input any Unicode really.
