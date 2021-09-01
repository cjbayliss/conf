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
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import qualified XMonad.StackSet as W

myLayoutsHook = spacingRaw False (Border 1 1 1 1) True (Border 1 1 1 1) True $
                lessBorders OnlyScreenFloat $
                onWorkspace "9" layoutFull $
                avoidStruts (layoutTCol ||| layoutGrid ||| layoutFull)
  where
    layoutTCol = ThreeCol 1 (3/100) (1/3)
    layoutGrid = Grid
    layoutFull = withBorder 0 Full

myManageHook = composeAll
               [ className =? "mpv" --> doF (W.view "9") <+> doShift "9" <+> doFullFloat
               , className =? "gmic_qt" --> doCenterFloat
               , className =? "gmic" --> doFloat
               , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" --> doFloat
               , isDialog --> doCenterFloat
               , isFullscreen --> doFullFloat ]

main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmonad/bar.hs"
  xmonad $ docks $ ewmh def
    { borderWidth = 1
    , modMask = mod4Mask
    , terminal = "xfce4-terminal"
    , normalBorderColor = "#808080"
    , focusedBorderColor = "#F44747"
    , handleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook
    , startupHook = do
        spawn "feh --no-fehbg --bg-fill --randomize ~/stuff/wallpapers/"
        spawn "xsetroot -cursor_name left_ptr"
    , manageHook = myManageHook
    , layoutHook = myLayoutsHook
    , logHook = dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppLayout = const ""
              , ppTitle = xmobarColor "#4EC9B0" "" . shorten 80 } >> ewmhDesktopsLogHook
    }
    `additionalKeysP`
    -- Despite the syntax, "M-<somekey>" doesn't do what you expect. At
    -- least if you're coming from Emacs. "M" is *not* meta, it's
    -- 'modMask', in my case the "Command/Super" key.
      [ ("M-r", restart "xmonad" True )
      -- adjust screen brightness
      , ("<XF86MonBrightnessDown>", spawn "light -U 5")
      , ("<XF86MonBrightnessUp>", spawn "light -A 5")
      -- alsa only systems
      , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+ unmute")
      , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%- unmute")
      , ("<XF86AudioMute>", spawn "amixer set Master toggle")
      -- control emms
      , ("<XF86AudioPlay>", spawn "playerctl play-pause")
      , ("<XF86AudioNext>", spawn "playerctl next")
      , ("<XF86AudioPrev>", spawn "playerctl previous")
      , ("M-d", spawn "rofi -combi-modi drun,window -show combi -modi combi")]
