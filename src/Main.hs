module Main where

import Main.Utf8 qualified as Utf8

{- |
 Main entry point.

 The `bin/run` script will invoke this function. See `.ghcid` file to change
 that.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    putStrLn "Hello 🌎"
