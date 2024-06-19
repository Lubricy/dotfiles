module Main where

import XMonad (xmonad, spawn)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)

import Lib (defaults)

main :: IO ()
main = do
  xmonad . docks . ewmh $ defaults
