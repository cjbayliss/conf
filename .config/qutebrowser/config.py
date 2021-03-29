import qutebrowser.api.interceptor

# poor person's adblock
def block_request(request: qutebrowser.api.interceptor.Request):
    if (  # youtube ads
        "youtube.com" in request.request_url.host()
        and "&adformat=" in request.request_url.query()
    ) or (  # animelab ads
        "animelab.com" in request.request_url.host()
        and "/qc.php" in request.request_url.path()
    ):
        request.block()


# rewrite url(s) (more later lol)
def rewrite_request(request: qutebrowser.api.interceptor.Request):
    if "reddit.com" in request.request_url.host():
        request.request_url.setHost("teddit.net")
        request.redirect(request.request_url)


qutebrowser.api.interceptor.register(block_request)
qutebrowser.api.interceptor.register(rewrite_request)
c.content.blocking.method = "both"

# load local config
config.load_autoconfig(False)

c.url.searchengines[
    "DEFAULT"
] = "https://duckduckgo.com/?q={}&kae=b&kak=-1&kax=-1&kaq=-1&kao=-1&kap=-1&kau=-1&kaj=m&k1=-1&ko=s&kac=1&kt=n&km=l&ka=n"

# stuff
c.content.autoplay = False
c.content.desktop_capture = False
c.content.geolocation = False
c.content.headers.do_not_track = False
c.content.javascript.prompt = False
c.content.media.audio_capture = False
c.content.media.audio_video_capture = False
c.content.media.video_capture = False
c.content.mouse_lock = False
c.content.notifications = False
c.content.persistent_storage = False
c.content.register_protocol_handler = False
c.content.ssl_strict = True
c.content.xss_auditing = True
c.downloads.location.directory = "$HOME/downloads"
c.downloads.location.prompt = False
c.prompt.filebrowser = False
c.tabs.show = "never"

# disable CVEs
c.content.javascript.enabled = False
# except for these sites...
config.set("content.javascript.enabled", True, "*://anilist.co/*")
config.set("content.javascript.enabled", True, "*://codeberg.org/*")
config.set("content.javascript.enabled", True, "*://discord.com/*")
config.set("content.javascript.enabled", True, "*://duckduckgo.com/*")
config.set("content.javascript.enabled", True, "*://github.com/*")
config.set("content.javascript.enabled", True, "*://gitlab.com/*")
config.set("content.javascript.enabled", True, "*://music.youtube.com/*")
config.set("content.javascript.enabled", True, "*://www.animelab.com/*")
config.set("content.javascript.enabled", True, "*://www.crunchyroll.com/*")
config.set("content.javascript.enabled", True, "*://www.youtube.com/*")
config.set("content.javascript.enabled", True, "chrome-devtools://*")
config.set("content.javascript.enabled", True, "chrome://*/*")
config.set("content.javascript.enabled", True, "devtools://*")
config.set("content.javascript.enabled", True, "qute://*/*")

# custom CSS (block ads, force better fonts, etc)
c.colors.webpage.bg = "#ededed"
c.content.user_stylesheets = "$HOME/.config/qutebrowser/default.css"

# editor command
c.editor.command = ["emacsclient", "{}"]

# default page
c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"

# softer status bar colors
c.colors.statusbar.normal.fg = "#f4f4f4"
c.colors.statusbar.url.success.https.fg = "#44bc44"

# fonts
c.fonts.default_family = "monospace"
c.fonts.default_size = "11pt"
c.fonts.web.family.fantasy = None
c.fonts.web.family.fixed = "Iosevka Fixed"
c.fonts.web.family.sans_serif = "Inter"
c.fonts.web.family.serif = "Inter"
c.fonts.web.family.standard = "Inter"
c.fonts.web.size.default = 17
c.fonts.web.size.default_fixed = 15
c.fonts.web.size.minimum = 15

# emacs style keybinds
c.bindings.default = {}

# input like normal
c.input.forward_unbound_keys = "all"
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.input.insert_mode.plugins = False

# bindings for normal mode
config.bind("0", "fake-key 0")
config.bind("1", "fake-key 1")
config.bind("2", "fake-key 2")
config.bind("3", "fake-key 3")
config.bind("4", "fake-key 4")
config.bind("5", "fake-key 5")
config.bind("6", "fake-key 6")
config.bind("7", "fake-key 7")
config.bind("8", "fake-key 8")
config.bind("9", "fake-key 9")
config.bind("<Alt+[>", "back")
config.bind("<Alt+]>", "forward")
config.bind("<Alt+w>", "yank selection")
config.bind("<Alt+x>", "set-cmd-text :")
config.bind("<Ctrl+Space>", "hint links tab-bg")
config.bind("<Ctrl+r>", "set-cmd-text ?")
config.bind("<Ctrl+s>", "set-cmd-text /")
config.bind("<Ctrl+x><Ctrl+->", "zoom-out")
config.bind("<Ctrl+x><Ctrl+=>", "zoom-in")
config.bind("<Ctrl+x><Ctrl+c>", "quit")
config.bind("<Ctrl+x><Ctrl+f>", "set-cmd-text -s :open -t")
config.bind("<Ctrl+x><Ctrl+l>", "config-source")
config.bind("<Ctrl+x><Left>", "tab-prev")
config.bind("<Ctrl+x><Right>", "tab-next")
config.bind("<Ctrl+x>b", "set-cmd-text -s :tab-select")
config.bind("<Ctrl+x>k<Return>", "tab-close")
config.bind("<Ctrl+x>l", "reload")
config.bind("<Ctrl+x>v", "hint links spawn mpv {hint-url}")
config.bind("<Ctrl+y>", "insert-text {clipboard}")

# javascript toggle
config.bind(
    "<Ctrl-x>jt",
    "config-cycle -p -t -u *://{url:host}/* content.javascript.enabled ;; reload",
)
config.bind(
    "<Ctrl-x>je",
    "config-cycle -p -u *://{url:host}/* content.javascript.enabled ;; reload",
)
config.bind(
    "<Ctrl-x>ja",
    "config-cycle -p -t -u *://*.{url:host}/* content.javascript.enabled ;; reload",
)

# bindings for command mode
config.bind("<Alt+n>", "command-history-next", mode="command")
config.bind("<Alt+p>", "command-history-prev", mode="command")
config.bind("<Ctrl+g>", "mode-leave", mode="command")
config.bind("<Ctrl+r>", "search-prev", mode="command")
config.bind("<Ctrl+s>", "search-next", mode="command")
config.bind("<Down>", "completion-item-focus next", mode="command")
config.bind("<Escape><Escape><Escape>", "mode-leave", mode="command")
config.bind("<Return>", "command-accept", mode="command")
config.bind("<Shift+Tab>", "completion-item-focus prev", mode="command")
config.bind("<Tab>", "completion-item-focus next", mode="command")
config.bind("<Up>", "completion-item-focus prev", mode="command")

# bindings for hint mode
config.bind("<Ctrl+g>", "mode-leave", mode="hint")
config.bind("<Escape><Escape><Escape>", "mode-leave", mode="hint")
config.bind("<Return>", "follow-hint", mode="hint")

# need to be able to leave insert mode because of !@#$%^ devtools
config.bind("<Ctrl+g>", "mode-leave", mode="insert")
config.bind("<Escape><Escape><Escape>", "mode-leave", mode="insert")

# bindings for prompt mode
config.bind("<Ctrl+g>", "mode-leave", mode="prompt")
config.bind("<Ctrl+x><Ctrl+n>", "prompt-accept no", mode="prompt")
config.bind("<Ctrl+x><Ctrl+y>", "prompt-accept yes", mode="prompt")
config.bind("<Down>", "prompt-item-focus next", mode="prompt")
config.bind("<Escape><Escape><Escape>", "mode-leave", mode="prompt")
config.bind("<Return>", "prompt-accept", mode="prompt")
config.bind("<Shift+Tab>", "prompt-item-focus prev", mode="prompt")
config.bind("<Tab>", "prompt-item-focus next", mode="prompt")
config.bind("<Up>", "prompt-item-focus prev", mode="prompt")
