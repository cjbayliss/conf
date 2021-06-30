import Data.Ratio -- this makes the '%' operator available (optional)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.StackSet as W

myLayoutsHook = spacingRaw False (Border 0 3 3 3) False (Border 3 3 3 3) True $
                onWorkspace "9" Full $
                lessBorders OnlyScreenFloat $
                layoutTall ||| layoutSpiral ||| layoutGrid ||| layoutMirror ||| layoutFull
  where
    layoutTall = Tall 1 (3/100) (1/2)
    layoutSpiral = spiral (125 % 146)
    layoutGrid = Grid
    layoutMirror = Mirror (Tall 1 (3/100) (3/5))
    layoutFull = Full

myManageHook = composeAll
               [ className =? "mpv" --> doF (W.view "9") <+> doShift "9"
               , isDialog --> doCenterFloat
               , isFullscreen --> doFullFloat ]

main = do
  xmonad $ ewmh def
    { borderWidth = 1
    , modMask = mod4Mask
    , terminal = "sakura"
    , normalBorderColor = "#251f2e"
    , focusedBorderColor = "#3e334d"
    , handleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook
    , startupHook = do
        spawn "feh --no-fehbg --bg-fill --randomize ~/stuff/wallpapers/"
        spawn "xsetroot -cursor_name left_ptr"
    , manageHook = myManageHook
    , layoutHook = myLayoutsHook
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
