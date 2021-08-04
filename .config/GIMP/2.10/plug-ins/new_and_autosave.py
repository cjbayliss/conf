#!/usr/bin/env python

from gimpfu import *
import datetime
import hashlib
import os
import time


def new_and_autosave():
    width = 2400
    height = 1800

    img = pdb.gimp_image_new(width, height, RGB)
    bg = pdb.gimp_layer_new(
        img, width, height, RGBA_IMAGE, "Background", 100, NORMAL_MODE
    )
    sketch = pdb.gimp_layer_new(
        img, width, height, RGBA_IMAGE, "Sketch", 100, NORMAL_MODE
    )
    pdb.gimp_image_insert_layer(img, bg, None, 0)
    pdb.gimp_image_insert_layer(img, sketch, None, 0)
    pdb.gimp_drawable_fill(bg, FILL_BACKGROUND)
    pdb.gimp_display_new(img)
    pdb.gimp_displays_flush()

    # generate filename
    img.filename = (
        os.environ["XDG_DOCUMENTS_DIR"]
        + "/art/"
        + str(datetime.date.today().year)
        + "/"
        + str(datetime.date.today().month).zfill(2)
        + "/"
        + str(datetime.date.today())
        + "-"
        + hashlib.sha1(str(time.time())).hexdigest()[0:8]
        + ".xcf"
    )

    while True:
        pdb.gimp_file_save(
            img,
            pdb.gimp_image_get_active_drawable(img),
            img.filename,
            img.filename,
        )
        time.sleep(2 * 60)


register(
    "new_and_autosave",
    "New Image, Save, & Autosave",
    "Create a New Image, Save, and Enable Autosave.",
    "Christopher Bayliss",
    "CC0-1.0",
    "2021",
    "New Image, Save, & Autosave",
    "",
    [],
    [],
    new_and_autosave,
    menu="<Image>/File/Create",
)

main()
