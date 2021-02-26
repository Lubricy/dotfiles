module Lib
    ( defaults
    ) where

import XMonad
import XMonad.Hooks.ManageHelpers
import Graphics.X11.ExtraTypes.XF86 ()
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.NoBorders (smartBorders)
-- import  System.Taffybar.Support.PagerHints (pagerHints)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myModMask = mod4Mask
myTerminal = "kitty"

myLayoutHook =  (tall ||| Mirror tall ||| Full)
                  where  tall = Tall 1 (3/100) (1/2)

myManageHook = composeAll . concat $
    [ [ className =? "Firefox"  --> doShift "3" ]
    , [ className =? "Emacs" <&&> not <$> title =? "doom-capture" --> doShift "2" ]
    , [ title =? "doom-capture" --> doRectFloat (W.RationalRect 0.2 0.2 0.6 0.6) ]
    , [(title =? "Picture-in-Picture") --> doRectFloat (W.RationalRect 0.7 0.1 0.2 0.2) ]
    , [ className =? "Thunderbird" --> doShift "5" ]
    , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat ]
    , [ className =? "Pidgin" --> doShift "5" ]
    , [ className =? "Gimp" --> doShift "6" ]
    , [ className =? f --> doFloat | f <- myFloatingWindows ] ]
    where
        myFloatingWindows   = [ "Pidgin", "Skype", "Galculator", "Lxappearance", "Transmission-gtk", "Vlc", "Gimp"]
        
myGaps = spacingRaw False screenBorder True windowBorder True 
  where screenBorder = Border 10 10 10 10
        windowBorder = Border 10 10 10 10
  
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
defaults = def {
  terminal = myTerminal
  , borderWidth = 2
  , focusedBorderColor = "#ffcc66"
  , normalBorderColor  = "#cccccc"
  , modMask = myModMask
  , layoutHook = avoidStruts . myGaps . smartBorders $ myLayoutHook
  , startupHook = spawn "~/.config/polybar/launch.sh"
  , manageHook = myManageHook
  } `additionalKeysP`
  [ ("M-<Return>", spawn myTerminal)
  , ("M-x", spawn "~/.emacs.d/bin/org-capture")
  , ("M-t", withFocused toggleFloat)
  , ("M-p", spawn "rofi -show run")
  , ("M-q", spawn "dunstify recompiling && xmonad --recompile && xmonad --restart && dunstify restarted")
  ]
