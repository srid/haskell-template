module Main where

import Main.Utf8 qualified as Utf8

data Example = Example
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Eq)

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    putTextLn "Hello 🌎 (from haskell-template)"
