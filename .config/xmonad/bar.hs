Config
  { font = "xft:Iosevka-10.5:semibold"
  , additionalFonts = ["xft:Iosevka-10.5:bold"]
    -- , borderColor = "#608B4E"
    -- , border = BottomB
  , bgColor = "#000006"
  , fgColor = "#DCDCE2"
  , position = TopW L 100
  , commands =
      [ Run
          ComX
          "sh"
          [ "-c"
          , "awk 'FNR==2 { e=1; print $1 }; END { exit !e }' /proc/net/arp"
          ]
          "DOWN"
          "arp"
          10
      , Run
          Com
          "sh"
          ["-c", "pactl list sinks | awk '/^[[:space:]]Volume:/ {print $5}'"]
          "volume"
          10
      , Run
          Com
          "sh"
          [ "-c"
          , "pactl list sinks | awk '/Mute:/ {print $2}' | sed -e 's/yes/AUDIO OFF /' -e 's/no//'"
          ]
          "audioStatus"
          10
      , Run Memory ["-t", "<used>M/<total>M (<usedratio>%)"] 10
      , Run Com "head" ["-c4", "/proc/loadavg"] "loadavg" 10
      , Run Date "%0e %^a %H:%M" "date" 10
      , Run StdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " %StdinReader% }{ <fc=#FF3600>%audioStatus%</fc>%volume% <fc=#36363a><</fc> <fc=#99A4BC>%memory%</fc> <fc=#36363a><</fc> <fc=#85CCBF>%arp%</fc> <fc=#36363a><</fc> %loadavg% <fc=#36363a><</fc> <fn=1><fc=#FFD392>%date%</fc></fn> "
  }
