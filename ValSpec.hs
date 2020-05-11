-- HSpec tests for Val.hs
-- Execute: runhaskell ValSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val

main :: IO ()
main = hspec $ do
  describe "strToVal" $ do
    it "converts an integer" $ do
        strToVal "2" `shouldBe` Integer 2

    it "converts a float" $ do
        strToVal "2.0" `shouldBe` Real 2.0

    it "converts a string" $ do
        strToVal "x2" `shouldBe` Id "x2"

  describe "toFloat" $ do  
      it "preserves real" $ do
          toFloat (Real 2.0) `shouldBe` (2.0::Float)
        
      it "converts integers" $ do
          toFloat (Integer 2) `shouldBe` (2.0::Float)

      it "errors on non-numbers" $ do
          -- this case is somewhat tricky
          evaluate (toFloat (Id "x")) `shouldThrow` errorCall "Not convertible to float"
