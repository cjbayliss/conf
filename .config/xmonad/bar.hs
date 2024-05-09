Config
  { font =
      "Iosevka, IPAGothic Regular 11.5, NanumGothicCoding Regular 11.5, Noto Color Emoji Regular 10, Regular 11.5"
  , dpi = 108
  , bgColor = "#000000"
  , fgColor = "#ffffff"
  , position = TopH 24
  , commands =
      [ Run
          ComX
          "sh"
          [ "-c"
          , "ip -j a | jq -r 'first(.[] | select(.operstate == \"UP\") | .addr_info[] | .local)'"
          ]
          "DOWN"
          "arp"
          10
      , Run
          Com
          "sh"
          ["-c", "pactl get-sink-volume @DEFAULT_SINK@ | awk '/Volume:/ {print $5}'"]
          "volume"
          10
      , Run
          Com
          "sh"
          [ "-c"
          , "pactl get-sink-mute @DEFAULT_SINK@ | sed -e 's/Mute: yes/AUDIO OFF/' -e 's/Mute: no//'"
          ]
          "audioStatus"
          10
      , Run
          Com
          "sh"
          [ "-c"
          , "bom"
          ]
          "temperature"
          9000
      , Run Memory ["-t", "<used>M/<total>M (<usedratio>%)"] 10
      , Run Com "head" ["-c4", "/proc/loadavg"] "loadavg" 10
      , Run Date "%0e %^a %H:%M" "date" 10
      , Run StdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " %StdinReader% }{ <fc=#ff8059>%audioStatus%</fc> %volume% <fc=#a8a8a8><</fc> <fc=#b0d6f5>%memory%</fc> <fc=#a8a8a8><</fc> <fc=#6ae4b9>%arp%</fc> <fc=#a8a8a8><</fc> %loadavg% <fc=#a8a8a8><</fc> <fc=#f8dec0>%temperature% %date%</fc> "
  }
