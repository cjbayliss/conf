My standard \*NIX config
========================

Some of it is nice, but most of it is boring.

ffmpeg
------

convert music to opus:

::

   for i in *.mp3; do ffmpeg -i "$i" -acodec libopus -b:a 256000 -vbr off "${i%.*}.opus"; done

cut out section:

::

   ffmpeg -i in.opus -ss 00:00:20 -to 00:00:40 -c copy out.opus
