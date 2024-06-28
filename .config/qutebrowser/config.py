# pylint: disable=C0114
import logging
import os
import re

from itertools import chain
from qutebrowser.api import interceptor

config = config  # pylint: disable=E0602,W0127
c = c  # pylint: disable=E0602,W0127

log = logging.getLogger()

# IMPORTANT: only matches whole domains
ADBLOCK = {
    "b.thumbs.redditmedia.com": [".css"],
    "googleads.g.doubleclick.net": ["googleads.g.doubleclick.net"],
    "www.youtube.com": [
        "&adformat=",
        "ads?",
        "adview?",
        "&el=adunit",
    ],
    "online.macquarie.com.au": ["background"],
}

# for regex, see https://docs.python.org/3/library/re.html#re.sub
REDIRECT = {
    "medium.com": {"type": "host", "host": "scribe.rip"},
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
    """block or redirect requests based on rules"""
    redirect = False
    initial_url = request.request_url.url()
    # poor person's adblock
    if request.request_url.host() in ADBLOCK:
        for pattern in ADBLOCK[request.request_url.host()]:
            if (
                pattern in request.request_url.query()
                or pattern in request.request_url.path()
                or pattern in request.request_url.host()
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
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2022.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2023.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2024.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/ubo-link-shorteners.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt",
    "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
    "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt",
]

# stuff
c.auto_save.session = True
c.content.autoplay = False
c.content.canvas_reading = False
c.content.cookies.accept = "never"
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

# use: /usr/share/qutebrowser/scripts/dictcli.py install en-AU
c.spellcheck.languages = ["en-AU"]

# list of internal pages
internal = [
    "chrome://*/*",
    "chrome-devtools://*",
    "devtools://*",
    "qute://*/*",
]

# list of shopping sites
shopping = [
    "*.bunnings.com.au",
    "*.catch.com.au",
    "*.coles.com.au",
    "*.computeralliance.com.au",
    "*.ebay.com.au",
    "*.gog.com",
    "*.igashop.com.au",
    "*.ikea.com",
    "*.instantscripts.com.au",
    "*.jaycar.com.au",
    "*.jbhifi.com.au",
    "*.kmart.com.au",
    "*.nintendo.com.au",
    "*.officeworks.com.au",
    "*.onepass.com.au",
    "*.pccasegear.com",
    "*.priceline.com.au",
    "*.scorptec.com.au",
    "*.target.com.au",
]

# list of financial sites
financial = [
    "*.macquarie.com.au",
    "*.paypal.com",
    "*.selfwealth.com.au",
]

# list of shipping sites
shipping = [
    "*.auspost.com.au",
    "*.couriersplease.com.au",
]

# list of entertainment sites
entertainment = [
    "*.anilist.co",
    "*.apple.com",
    "*.crunchyroll.com",
    "*.twitch.tv",
    "*.youtube.com",
]

# list of social sites
social = [
    "*.discord.com",
]

# list of development sites
devel = [
    "*.codeberg.org",
    "*.gentoo.org",
    "*.github.com",
    "*.gitlab.com",
    "*.sr.ht",
]

# other sites
other = [
    "*.bitwarden.com",
    "*.icloud.com",
]

# disable CVEs
c.content.javascript.enabled = False
# except for these sites...
ALLOW_SCRIPTS_COOKIES = list(
    chain.from_iterable(
        [
            devel,
            entertainment,
            financial,
            internal,
            other,
            shipping,
            shopping,
            social,
        ]
    )
)

for site in ALLOW_SCRIPTS_COOKIES:
    config.set("content.javascript.enabled", True, site)
    config.set("content.cookies.accept", "no-3rdparty", site)

# reddit changes the page content if the referer is google.com
config.set("content.headers.custom", {"Referer": ""}, "*://old.reddit.com/r/*")

# darkmode
c.colors.webpage.bg = "#111"
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"

DISABLE_DARKMODE = [
    "*.codeberg.org",
    "*.crunchyroll.com",
    "*.discord.com",
    "*.github.com",
    "*.lobste.rs",
    "*.sr.ht",
    "*.twitch.tv",
    "*.youtube.com",
]

for site in DISABLE_DARKMODE:
    config.set("colors.webpage.darkmode.enabled", False, site)

# custom CSS (block ads, force better fonts, etc)
c.content.user_stylesheets = f"{os.environ['HOME']}/.config/qutebrowser/default.css"

# editor command
c.editor.command = ["alacritty", "-e", "kak", "{}"]

# default page
c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"

# default search engine
c.url.searchengines = {
    "am": "https://ask.moe/search?q={}",
    "DEFAULT": "https://www.google.com/search?q={}&gbv=1",
    "dp": "https://packages.debian.org/search?keywords={}&searchon=names&section=all",
    "eb": "https://www.ebay.com.au/sch/i.html?_nkw={}",
    "gb": "https://bugs.gentoo.org/buglist.cgi?quicksearch={}",
    "gh": "https://github.com/search?q={}",
    "gp": "https://packages.gentoo.org/packages/search?q={}",
    "gw": "https://wiki.gentoo.org/index.php?search={}",
    "mwd": "https://www.merriam-webster.com/dictionary/{}",
    "no": "https://search.nixos.org/packages?channel=unstable&query={}",
    "np": "https://search.nixos.org/options?channel=unstable&query={}",
    "posix": "https://pubs.opengroup.org/onlinepubs/9699919799/utilities/{}.html",
    "wd": "https://en.wiktionary.org/wiki/Special:Search?search={}",
    "wiki": "https://en.wikipedia.org/wiki/Special:Search?search={}",
    "ym": "https://music.youtube.com/search?q={}",
    "yt": "https://youtube.com/results?search_query={}",
}
c.completion.web_history.exclude = [
    "https://duckduckgo.com",
    "https://*.google.com",
    "https://*.google.com.au",
]

# colors
c.colors.statusbar.command.bg = "#222"
c.colors.statusbar.normal.bg = "#222"
c.colors.statusbar.url.error.fg = "#f77"
c.colors.statusbar.url.hover.fg = "#7ff"
c.colors.statusbar.url.success.http.fg = "#7f7"
c.colors.statusbar.url.success.https.fg = "#7f7"
c.colors.statusbar.url.warn.fg = "#ff7"

c.colors.tabs.bar.bg = "#000"
c.colors.tabs.even.bg = "#000"
c.colors.tabs.indicator.start = "#ffa"
c.colors.tabs.odd.bg = "#000"
c.colors.tabs.pinned.even.bg = "#424"
c.colors.tabs.pinned.odd.bg = "#242"
c.colors.tabs.pinned.selected.even.bg = "#222"
c.colors.tabs.pinned.selected.odd.bg = "#222"
c.colors.tabs.selected.even.bg = "#222"
c.colors.tabs.selected.odd.bg = "#222"

c.colors.completion.category.bg = "#222"
c.colors.completion.even.bg = "#111"
c.colors.completion.odd.bg = "#111"

# various UI settings
c.tabs.max_width = 200
c.completion.scrollbar.padding = 0
c.completion.scrollbar.width = 0
c.completion.shrink = True
c.statusbar.position = "top"

# fonts
c.fonts.default_family = "monospace"
c.fonts.default_size = "11pt"
c.fonts.web.size.default = 17
c.fonts.web.size.minimum = 15

# keybinds
config.bind("b", "cmd-set-text -s :tab-select")
config.bind("<Ctrl-Escape>", "fake-key <Escape>")
config.bind("<Ctrl+t>", "cmd-set-text -s :open -t ")
config.bind("gn", "tab-next")
config.bind("gp", "tab-prev")
config.bind("J", "scroll-page 0 0.5")
config.bind("K", "scroll-page 0 -0.5")
config.bind(";m", "hint links spawn mpv {hint-url}")
config.bind(";M", "spawn mpv {url}")
config.bind("<Shift-Escape>", "fake-key <Shift-Escape>")
config.bind("tt", "fake-key t")
