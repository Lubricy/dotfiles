module Lib
    ( defaults
    ) where

import XMonad
import XMonad.Hooks.ManageHelpers
import Graphics.X11.ExtraTypes.XF86 ()
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, checkDock, ToggleStruts(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.EwmhDesktops
import Data.List (find)
import XMonad.Layout.MultiToggle ((??), mkToggle, Toggle(..), EOT(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL, NOBORDERS))
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myModMask = mod4Mask
myTerminal = "kitty"

-- EOT :: End of Transformer
myLayoutHook = mkToggle (NOBORDERS ?? FULL ?? EOT) (
    tall
    ||| Mirror tall
    ||| Full)
  where tall = Tall 1 (3/100) (1/2)

-- | Restack dock under lowest managed window.
lowerDock :: ManageHook
lowerDock = checkDock --> do
    w <- ask
    mlw <- liftX $ findLowest
    case mlw of
      Just lw   -> liftX $ do
        d <- asks display
        liftIO $ restackWindows d [lw, w]
        return idHook
      Nothing   -> return idHook

-- | Find lowest managed window.
findLowest :: X (Maybe Window)
findLowest  = withWindowSet $ \ws -> do
    d <- asks display
    r <- asks theRoot
    (_, _, ts) <- liftIO $ queryTree d r
    return (find (`W.member` ws) ts)

fullscreen :: X ()
fullscreen = do
  sendMessage $ ToggleStruts
  toggleScreenSpacingEnabled
  toggleWindowSpacingEnabled
  sendMessage $ Toggle FULL

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
 modMask = myModMask
  , terminal = myTerminal
  , borderWidth = 2
  , focusedBorderColor = "#ffcc66"
  , normalBorderColor  = "#cccccc"
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  , layoutHook =  avoidStruts . myGaps . smartBorders $ myLayoutHook
  , startupHook = spawn "~/.config/polybar/launch.sh"
  , manageHook = lowerDock <+> manageDocks <+> myManageHook
  } `additionalKeysP`
  [ ("M-<Return>", spawn myTerminal)
  , ("M-d", spawn "~/.emacs.d/bin/org-capture")
  , ("M-t", withFocused toggleFloat)
  , ("M-w", windows W.focusMaster)
  , ("M-p", spawn "rofi -show run")
  , ("M-x", kill)
  , ("M-r", refresh)
  , ("M-f", fullscreen)
  , ("M-q", spawn "dunstify recompiling && xmonad --recompile && xmonad --restart && dunstify restarted")
  ]

