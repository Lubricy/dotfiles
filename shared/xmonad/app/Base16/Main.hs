{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Text
import Text.Megaparsec
import Text.Mustache
import qualified Data.Text.Lazy.IO as TO

main :: IO ()
main = do
  let res = compileMustacheText "foo"
        "Hi, {{name}}! You have:\n{{#things}}\n  * {{.}}\n{{/things}}\n"
  case res of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right template -> TO.putStr $ renderMustache template $ object
      [ "name"   .= ("John" :: Text)
      , "things" .= ["pen" :: Text, "candle", "egg"]
      ]
