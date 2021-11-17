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
import XMonad.Util.Run

myLayoutsHook =
  spacingRaw False (Border 1 1 1 1) True (Border 1 1 1 1) True $
  avoidStruts (ThreeCol 1 (3 / 100) (1 / 3) ||| Grid ||| Full)

myManageHook =
  composeAll
    [ className =? "mpv" --> doF (W.view "9") <+> doShift "9"
    , className =? "gmic_qt" --> doCenterFloat
    , className =? "gmic" --> doFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" -->
      doFloat
    , isDialog --> doCenterFloat
    ]

main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmonad/bar.hs"
  xmonad $
    docks $
    ewmh
      def
        { borderWidth = 1
        , modMask = mod4Mask
        , normalBorderColor = "#444444"
        , focusedBorderColor = "#b6a0ff"
        , startupHook =
            do spawn "hsetroot -solid gray10"
               spawn "feh --no-fehbg --bg-fill ~/stuff/wallpapers/default"
               spawn "xsetroot -cursor_name left_ptr"
        , manageHook = myManageHook
        , layoutHook = myLayoutsHook
        , logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppLayout = const ""
                , ppTitle = xmobarColor "#b0d6f5" "" . shorten 80
                , ppCurrent = xmobarColor "#b6a0ff" "" . wrap "[" "]"
                } >>
            ewmhDesktopsLogHook
        } `additionalKeysP`
    -- Despite the syntax, "M-<somekey>" doesn't do what you expect. At
    -- least if you're coming from Emacs. "M" is *not* meta, it's
    -- 'modMask', in my case the "Command/Super" key.
    [ ("M-r", restart "xmonad" True)
      -- adjust screen brightness
    , ("<XF86MonBrightnessDown>", spawn "light -U 5")
    , ("<XF86MonBrightnessUp>", spawn "light -A 5")
      -- using pactl with pipewire
    , ( "<XF86AudioRaiseVolume>"
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ( "<XF86AudioLowerVolume>"
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
      -- control multimedia
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ( "M-p"
      , spawn
          "dmenu_run -fn 'Iosevka-10.5:semibold' -nb '#000' -nf '#fff' -sb '#000' -sf '#b6a0ff'")
    ]
