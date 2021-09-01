Config
    { font = "xft:Iosevka-10.5:semibold"
    , additionalFonts = ["xft:Iosevka-10.5:bold"]
    -- , borderColor = "#608B4E"
    -- , border = BottomB
    , bgColor = "#0f0f0f"
    , fgColor = "#d4d4d4"
    , position = TopW L 100
    , commands =
          [ Run ComX "sh" ["-c", "tail -n1 /proc/net/arp | cut -d\" \" -f1"] "DOWN" "arp" 10
          , Run Alsa
                "default"
                "Master"
                ["-t", "<status>VOL: <volume>%", "--", "-O", "", "-o", "AUDIO OFF ", "-c", "#F44747"]
          , Run Memory ["-t", "<used>M/<total>M (<usedratio>%)"] 10
          , Run Com "head" ["-c4", "/proc/loadavg"] "loadavg" 10
          , Run Date "%0e %^a %H:%M" "date" 10
          , Run StdinReader
          ]
    , sepChar = "%"
    , alignSep = "}{"
    , template =
          " %StdinReader% }{ <fc=#569CD6>%alsa:default:Master%</fc> <fc=#333333>│</fc> <fc=#608B4E>%memory%</fc> <fc=#333333>│</fc> <fc=#C678DD>%arp%</fc> <fc=#333333>│</fc> %loadavg% <fc=#333333>│</fc> <fn=1><fc=#DCDCAA>%date%</fc></fn> "
    }
