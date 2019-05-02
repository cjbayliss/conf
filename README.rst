My standard GNU/Linux config
============================

Some of it is nice, but most of it is boring.

In 2019 I re-created this repo, deleting all the old history as it was mostly
useless to me. This makes cloning quicker (was already fast).  Obviously I kept
a backup, if you need something that you think was in an old commit, email
me. :)

ffmpeg
------

convert music to opus:

::

    for i in *.mp3; do ffmpeg -i "$i" -acodec libopus -b:a 256000 -vbr off "${i%.*}.opus"; done

cut out section:

::

    ffmpeg -i in.opus -ss 00:00:20 -to 00:00:40 -c copy out.opus

input emoji and japanese
------------------------

If you are in emacs toggle the input method with C-\\, and C-x RET C-\\ to
select a different input.

Install ``ibus``, ``ibus-anthy``, and on debian ``ibus-table-emoji``.

Restart the session, and the config will be loaded.

Run ``ibus-setup`` to configure ``ibus`` and add the ``anthy`` input.
``~/.bin/ibus-toggle`` is bound to Cmd+Space in the sway config. Set the emoji
input to something like Cmd+e and the unicode input to something like Cmd+u.

~/.emac.d
---------

install stuff, but don't load melpa each time you run emacs:

.. code:: elisp

      (defun setup-emacs ()
        "install emacs packages"
        (interactive)
        (require 'package)
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
        (package-refresh-contents)
        ;; this is just an example...
        (if (not (package-installed-p 'erc-hl-nicks))
            (progn (package-install 'erc-hl-nicks))))
