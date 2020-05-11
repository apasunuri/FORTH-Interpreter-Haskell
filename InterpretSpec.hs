-- HSpec tests for Val.hs
-- Execute: runhaskell InterpretSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval
import Interpret

main :: IO ()
main = hspec $ do
  describe "evalF" $ do
    it "preserves output for numbers" $ do
        evalF ([], "x") (Real 3.0) `shouldBe` ([Real 3.0], "x")

    it "passes through operators" $ do
        evalF ([Real 2.2, Integer 2], "") (Id "*") `shouldBe` ([Real 4.4], "")

    it "propagates output" $ do
        evalF ([Integer 2], "") (Id ".") `shouldBe` ([],"2")    

  describe "interpret" $ do
    context "RPN" $ do
        it "multiplies two integers" $ do
            interpret "2 3 *" `shouldBe` ([Integer 6], "")      

        -- numerical precision makes this tricky
        it "multiplies floats and integers" $ do
            interpret "2 2.2 3.4 * *" `shouldBe` ([Real 14.960001], "")

    context "Printout" $ do
        it "computes product and outputs" $ do
            interpret "2 6 * ." `shouldBe` ([], "12")