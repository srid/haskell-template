{-# LANGUAGE TypeApplications #-}

module Main where

import Main.Utf8 (withUtf8)

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  withUtf8 $ do
    putStrLn "Hello 🌎"
