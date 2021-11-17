Config
  { font =
      "xft:Iosevka-10.5:semibold,IPAGothic:size=10.5:style=Regular,Baekmuk Gulim:size=10.5:style=Regular,Noto Color Emoji:size=10:style=Regular"
  , bgColor = "#000000"
  , fgColor = "#ffffff"
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
      " %StdinReader% }{ <fc=#ff8059>%audioStatus%</fc>%volume% <fc=#a8a8a8><</fc> <fc=#b0d6f5>%memory%</fc> <fc=#a8a8a8><</fc> <fc=#6ae4b9>%arp%</fc> <fc=#a8a8a8><</fc> %loadavg% <fc=#a8a8a8><</fc> <fc=#f8dec0>%date%</fc> "
  }
