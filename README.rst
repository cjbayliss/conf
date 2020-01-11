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
