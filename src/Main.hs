{-# LANGUAGE TypeApplications #-}

module Main where

import Main.Utf8 (withUtf8)

main :: IO ()
main = do
  withUtf8 $ do
    putStrLn "Hello World"

