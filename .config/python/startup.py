# if you are a python developer or someone else considering writing
# files into peoples home directory, PLEASE allow people to at least
# turn it off, at a bare minimum.  providing a way to change where the
# files are written is polite too!
#
# no, readline.set_auto_history(False) doesnt stop python writing
# ~/.python_history, it just stops python adding history to that file.
#
# refs:
#
#   * https://bugs.python.org/issue5845
#   * https://bugs.python.org/issue20886
#   * https://bugs.python.org/issue26870
#   * https://bugs.python.org/issue29779
#   * https://bugs.python.org/issue41563
#   * https://docs.python.org/3/library/readline.html#example
#   * https://github.com/python/cpython/pull/13208
#   * https://unix.stackexchange.com/a/297834

try:
    import readline
    readline.write_history_file = lambda *args: None
    del readline
except ImportError:
    pass
