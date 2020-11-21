module Main where

import XMonad (xmonad)
import XMonad.Hooks.EwmhDesktops (ewmh)

import Lib (defaults)

main :: IO ()
main = do
  xmonad $ ewmh defaults
