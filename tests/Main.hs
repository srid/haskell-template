module Main where

import Lib qualified
import Test.Hspec (describe, hspec, it, shouldContain)

main :: IO ()
main = hspec $ do
  describe "Lib.hello" $ do
    it "contains the world emoji" $ do
      toString Lib.hello `shouldContain` "🌎"
