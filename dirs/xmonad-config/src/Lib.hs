module Lib
    ( defaults
    ) where

import XMonad

defaults :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaults = def
