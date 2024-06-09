from qutebrowser.api import interceptor
from urllib.parse import parse_qs
import logging
import os
import re
import requests
import secrets
import sys
import time

log = logging.getLogger()


# IMPORTANT: only matches whole domains
ADBLOCK = {
    "b.thumbs.redditmedia.com": ".css",
    "googleads.g.doubleclick.net": "googleads.g.doubleclick.net",
    "www.youtube.com": "&adformat=",
    "www.youtube.com": "ads?",
    "www.youtube.com": "adview?",
    "www.youtube.com": "&el=adunit",
}

# for regex, see https://docs.python.org/3/library/re.html#re.sub
REDIRECT = {
    "www.reddit.com": {"type": "host", "host": "old.reddit.com"},
    "www.google.com": {
        "type": "regex",
        "pattern": r"(.*\/url\?q\=)(.*?)&.*",
        "repl": r"\2",
    },
    "www.amazon.com": {
        "type": "regex",
        "pattern": r"(\w+:\/\/.*?\.amazon\..*?\/).*(dp\/.*?\/).*",
        "repl": r"\1\2",
    },
    "www.amazon.com.au": {
        "type": "regex",
        "pattern": r"(\w+:\/\/.*?\.amazon\..*?\/).*(dp\/.*?\/).*",
        "repl": r"\1\2",
    },
}


def request_manager(request: interceptor.Request) -> None:
    redirect = False
    initial_url = request.request_url.url()
    # poor person's adblock
    if request.request_url.host() in ADBLOCK and (
        ADBLOCK[request.request_url.host()] in request.request_url.query()
        or ADBLOCK[request.request_url.host()] in request.request_url.path()
        or ADBLOCK[request.request_url.host()] in request.request_url.host()
    ):
        log.info("BLOCKED: %s", request.request_url.url())
        request.block()

    # upgrade to https
    if request.request_url.scheme() == "http":
        request.request_url.setScheme("https")
        log.info("UPGRADING_TO_HTTPS: %s", request.request_url.url())
        redirect = True

    # redirector
    if (
        request.request_url.host() in REDIRECT
        and REDIRECT[request.request_url.host()]["type"] == "host"
    ):
        request.request_url.setHost(REDIRECT[request.request_url.host()]["host"])
        log.info("REDIRECTING: %s -> %s", initial_url, request.request_url.url())
        redirect = True

    if (
        request.request_url.host() in REDIRECT
        and REDIRECT[request.request_url.host()]["type"] == "regex"
        and (
            re.sub(
                REDIRECT[request.request_url.host()]["pattern"],
                REDIRECT[request.request_url.host()]["repl"],
                request.request_url.url(),
            )
            != initial_url
        )
    ):
        request.request_url.setUrl(
            re.sub(
                REDIRECT[request.request_url.host()]["pattern"],
                REDIRECT[request.request_url.host()]["repl"],
                request.request_url.url(),
            )
        )
        log.info("REDIRECTING: %s -> %s", initial_url, request.request_url.url())
        redirect = True

    if redirect:
        try:
            request.redirect(request.request_url)
        except:
            # don't crash the browser
            pass


interceptor.register(request_manager)

# don't load local config
config.load_autoconfig(False)

c.content.blocking.adblock.lists = [
    "https://easylist.to/easylist/easylist.txt",
    "https://easylist.to/easylist/easyprivacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt",
    "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
    "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt",
]

# stuff
c.auto_save.session = True
c.content.autoplay = False
c.content.canvas_reading = False
c.content.cookies.accept = "no-3rdparty"
c.content.desktop_capture = False
c.content.dns_prefetch = False
c.content.geolocation = False
c.content.headers.do_not_track = False
c.content.mouse_lock = False
c.content.notifications.enabled = False
c.content.persistent_storage = False
c.content.register_protocol_handler = False
c.content.xss_auditing = True
c.downloads.location.directory = "$HOME/stuff/downloads"
c.downloads.location.prompt = False
c.prompt.filebrowser = False
c.tabs.show = "never"

# use: /usr/share/qutebrowser/scripts/dictcli.py install en-AU
c.spellcheck.languages = ["en-AU"]

# disable CVEs
c.content.javascript.enabled = False
# except for these sites...
ALLOW_SCRIPTS = [
    "*://*.amazon.com/*",
    "*://*.amazon.com.au/*",
    "*://anilist.co/*",
    "*://codeberg.org/*",
    "*://discord.com/*",
    "*://*.ebay.com/*",
    "*://*.ebay.com.au/*",
    "*://github.com/*",
    "*://gitlab.com/*",
    "*://music.youtube.com/*",
    "*://*.sr.ht/*",
    "*://www.crunchyroll.com/*",
    "*://www.twitch.tv/*",
    "*://www.youtube.com/*",
    "*://www.youtube-nocookie.com/embed/*",
    "chrome://*/*",
    "chrome-devtools://*",
    "devtools://*",
    "qute://*/*",
]

for site in ALLOW_SCRIPTS:
    config.set("content.javascript.enabled", True, site)

# reddit changes the page content if the referer is google.com
config.set("content.headers.custom", {"Referer": ""}, "*://old.reddit.com/r/*")

# darkmode
c.colors.webpage.bg = "#111"
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"

DISABLE_DARKMODE = [
    "*://codeberg.org/*",
    "*://discord.com/*",
    "*://github.com/*",
    "*://lobste.rs/*",
    "*://*.sr.ht/*",
    "*://www.crunchyroll.com/*",
    "*://www.twitch.tv/*",
    "*://*.youtube.com/*",
]

for site in DISABLE_DARKMODE:
    config.set("colors.webpage.darkmode.enabled", False, site)

# custom CSS (block ads, force better fonts, etc)
c.content.user_stylesheets = f"{os.environ['HOME']}/.config/qutebrowser/default.css"

# editor command
c.editor.command = ["foot", "kak", "{}"]

# default page
c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"

# default search engine
c.url.searchengines = {
    "!a": "https://www.amazon.com.au/s?k={}",
    "!am": "https://ask.moe/search?q={}",
    "DEFAULT": "https://www.google.com/search?q={}&gbv=1",
    "!dp": "https://packages.debian.org/search?keywords={}&searchon=names&section=all",
    "!eb": "https://www.ebay.com.au/sch/i.html?_nkw={}",
    "!gb": "https://bugs.gentoo.org/buglist.cgi?quicksearch={}",
    "!gh": "https://github.com/search?q={}",
    "!gp": "https://packages.gentoo.org/packages/search?q={}",
    "!gw": "https://wiki.gentoo.org/index.php?search={}",
    "!mwd": "https://www.merriam-webster.com/dictionary/{}",
    "!np": "https://search.nixos.org/options?channel=unstable&query={}",
    "!np": "https://search.nixos.org/packages?channel=unstable&query={}",
    "posix": "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/{}.html",
    "!up": "https://packages.ubuntu.com/search?keywords={}",
    "!wd": "https://en.wiktionary.org/wiki/Special:Search?search={}",
    "!w": "https://en.wikipedia.org/wiki/Special:Search?search={}",
    "!ym": "https://music.youtube.com/search?q={}",
    "!yt": "https://youtube.com/results?search_query={}",
}

c.colors.completion.category.bg = "#222"
c.colors.completion.even.bg = "#111"
c.colors.completion.odd.bg = "#111"

c.completion.scrollbar.padding = 0
c.completion.scrollbar.width = 0
c.completion.shrink = True
c.statusbar.position = "top"

# fonts
c.fonts.default_family = "monospace"
c.fonts.default_size = "11pt"
c.fonts.web.size.default = 17
c.fonts.web.size.minimum = 15

# clear default keybinds
c.bindings.default = {}

# input like normal
c.input.forward_unbound_keys = "all"
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
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
config.bind("<Shift-Escape>", "fake-key <Shift-Escape>")

# general keybinds
config.bind("<Ctrl+w>", "tab-close")
config.bind("<Ctrl+;>", "cmd-set-text :")

config.bind("<Ctrl+Shift+l>", "cmd-set-text -s :open")
config.bind("<Ctrl+l>", "cmd-set-text -s :open {url}")
config.bind("<Ctrl+t>", "cmd-set-text -s :open -t")

config.bind("<Ctrl+->", "zoom-out")
config.bind("<Ctrl+0>", "zoom 100")
config.bind("<Ctrl+=>", "zoom-in")

config.bind("<Ctrl+b>c", "config-source")
config.bind("<Ctrl+b><Ctrl+c>", "config-source")
config.bind("<Ctrl+r>", "reload")

config.bind("<Alt+0>", "tab-focus 10")
config.bind("<Alt+1>", "tab-focus 1")
config.bind("<Alt+2>", "tab-focus 2")
config.bind("<Alt+3>", "tab-focus 3")
config.bind("<Alt+4>", "tab-focus 4")
config.bind("<Alt+5>", "tab-focus 5")
config.bind("<Alt+6>", "tab-focus 6")
config.bind("<Alt+7>", "tab-focus 7")
config.bind("<Alt+8>", "tab-focus 8")
config.bind("<Alt+9>", "tab-focus 9")

config.bind("<Ctrl+f>", "cmd-set-text /")
config.bind("<Ctrl+b><Ctrl+y>", "yank")

config.bind("<Ctrl+b><Ctrl+m>", "hint links spawn mpv {hint-url}")
config.bind("<Ctrl+b><Ctrl+M>", "spawn mpv {url}")
config.bind("<Ctrl+b>m", "hint links spawn mpv {hint-url}")
config.bind("<Ctrl+b>M", "spawn mpv {url}")

config.bind("<Ctrl+i>", "devtools bottom")

config.bind("<Ctrl+Left>", "tab-prev")
config.bind("<Ctrl+Right>", "tab-next")

config.bind("<Ctrl+Down>", "scroll-page 0 0.7")
config.bind("<Ctrl+Up>", "scroll-page 0 -0.7")

# javascript toggle
config.bind(
    "<Ctrl-b>jt",
    "config-cycle -p -t -u *://{url:host}/* content.javascript.enabled ;; reload",
)
config.bind(
    "<Ctrl-b>je",
    "config-cycle -p -u *://{url:host}/* content.javascript.enabled ;; reload",
)
config.bind(
    "<Ctrl-b>ja",
    "config-cycle -p -t -u *://*.{url:host}/* content.javascript.enabled ;; reload",
)

# bindings for command mode
config.bind("<Ctrl+f>", "search-next", mode="command")
config.bind("<Ctrl+g>", "mode-leave", mode="command")
config.bind("<Escape>", "mode-leave", mode="command")

config.bind("<Down>", "completion-item-focus next ;; command-history-next", mode="command")
config.bind("<Up>", " command-history-prev ;; completion-item-focus prev", mode="command")
config.bind("<Return>", "command-accept", mode="command")
config.bind("<Shift+Tab>", "completion-item-focus prev", mode="command")
config.bind("<Tab>", "completion-item-focus next", mode="command")

# bindings for hint mode
config.bind("<Ctrl+g>", "mode-leave", mode="hint")
config.bind("<Escape>", "mode-leave", mode="hint")
config.bind("<Return>", "follow-hint", mode="hint")

# need to be able to leave insert mode because of !@#$%^ devtools
config.bind("<Ctrl+g>", "mode-leave", mode="insert")
config.bind("<Escape>", "mode-leave", mode="insert")

# bindings for prompt mode
config.bind("<Ctrl+g>", "mode-leave", mode="prompt")
config.bind("<Down>", "prompt-item-focus next", mode="prompt")
config.bind("<Escape>", "mode-leave", mode="prompt")
config.bind("<Return>", "prompt-accept", mode="prompt")
config.bind("<Shift+Tab>", "prompt-item-focus prev", mode="prompt")
config.bind("<Tab>", "prompt-item-focus next", mode="prompt")
config.bind("<Up>", "prompt-item-focus prev", mode="prompt")
config.bind("n", "prompt-accept no", mode="prompt")
config.bind("y", "prompt-accept yes", mode="prompt")

# bindings for yesno mode
config.bind("<Ctrl+g>", "mode-leave", mode="yesno")
config.bind("<Escape>", "mode-leave", mode="yesno")
config.bind("<Return>", "prompt-accept", mode="yesno")
config.bind("N", "prompt-accept --save no", mode="yesno")
config.bind("Y", "prompt-accept --save yes", mode="yesno")
config.bind("n", "prompt-accept no", mode="yesno")
config.bind("y", "prompt-accept yes", mode="yesno")
