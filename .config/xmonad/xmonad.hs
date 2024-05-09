import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Run

myLayoutsHook =
  spacingRaw False (Border 1 1 1 1) True (Border 1 1 1 1) True $
  avoidStruts (ThreeCol 1 (3 / 100) (1 / 3) ||| Grid ||| Full)

myManageHook =
  composeAll
    [ className =? "mpv" --> doF (W.view "9") <+> doShift "9"
    , className =? "gmic_qt" --> doCenterFloat
    , className =? "gmic" --> doFloat
    , className =? "awakened-poe-trade" --> doFloat
    , title =? "Wine System Tray" --> doShift "8"
    , title =? "Battle.net" --> doF(W.view "4") <+> doShift "4"
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" -->
      doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" -->
      doRectFloat (W.RationalRect 0.05 0.05 0.9 0.9)
    , isDialog --> doCenterFloat
    ]

main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmonad/bar.hs"
  xmonad $
    docks $
    ewmh
      def
        { borderWidth = 2
        , modMask = mod4Mask
        , normalBorderColor = "#444444"
        , focusedBorderColor = "#b6a0ff"
        , terminal = "alacritty"
        , startupHook =
            do spawn "hsetroot -solid gray10"
               spawn "xsetroot -cursor_name left_ptr"
        , manageHook = myManageHook
        , layoutHook = myLayoutsHook
        , handleEventHook =
            handleEventHook def <+> Hacks.windowedFullscreenFixEventHook
        , logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppLayout = const ""
                , ppTitle = xmobarColor "#b0d6f5" "" . shorten 80
                , ppCurrent = xmobarColor "#b6a0ff" "" . wrap "[" "]"
                }
        } `additionalKeysP`
    -- Despite the syntax, "M-<somekey>" doesn't do what you expect. At
    -- least if you're coming from Emacs. "M" is *not* meta, it's
    -- 'modMask', in my case the "Command/Super" key.
    [ ("M-r", restart "xmonad" True)
      -- adjust screen brightness
    -- , ("<XF86MonBrightnessDown>", spawn "light -U 5")
    -- , ("<XF86MonBrightnessUp>", spawn "light -A 5")
      -- using pactl with pipewire
    , ( "<XF86AudioRaiseVolume>"
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ( "<XF86AudioLowerVolume>"
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("M--", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ("M-=", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ("M-m", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
      -- control multimedia
    , ("M-[", spawn "playerctl previous")
    , ("M-\\", spawn "playerctl play-pause")
    , ("M-]", spawn "playerctl next")
    , ( "M-s"
      , spawn
          "scrot -s -f ~/pictures/screenshots/%Y-%m-%d-%H%M%S-screenshot.png")
    , ("M-p", spawn "dmenu-apps")
    ]
