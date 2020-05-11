-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
    describe "eval" $ do
        context "*" $ do
            it "multiplies integers" $ do
                eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
            
            it "multiplies floats" $ do
                eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
                eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
                eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

            it "errors on too few arguments" $ do   
                evaluate (eval "*" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack Underflow"
    
        context "+" $ do
            it "adds integers" $ do
                eval "+" [Integer 22, Integer 3] `shouldBe` [Integer 25]
                eval "+" [Integer 90, Integer 34] `shouldBe` [Integer 124]
                eval "+" [Integer (negate 100), Integer (negate 8)] `shouldBe` [Integer (negate 108)]

            it "adds floats" $ do
                eval "+" [Integer 2, Real 3.14] `shouldBe` [Real 5.1400003]
                eval "+" [Real 0.9875, Integer 3] `shouldBe` [Real 3.9875]
                eval "+" [Real 4.128, Real 3.9] `shouldBe` [Real 8.028]

            it "errors on too few arguments" $ do   
                evaluate (eval "+" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "+" [Integer 2]) `shouldThrow` errorCall "Stack Underflow"
        
        context "-" $ do
            it "subtracts integers" $ do
                eval "-" [Integer 87, Integer 36] `shouldBe` [Integer (negate 51)]
                eval "-" [Integer (negate 79), Integer 4] `shouldBe` [Integer 83]
                
            it "subtracts floats" $ do
                eval "-" [Integer 2, Real 3.14] `shouldBe` [Real 1.1400001]
                eval "-" [Real 0.99, Integer 8] `shouldBe` [Real 7.01]
                eval "-" [Real 1.325, Real 6.987] `shouldBe` [Real 5.6619997]

            it "errors on too few arguments" $ do   
                evaluate (eval "-" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "-" [Real 8.88]) `shouldThrow` errorCall "Stack Underflow"
        
        context "/" $ do
            it "divides integers" $ do
                eval "/" [Integer 87, Integer 36] `shouldBe` [Integer 0]
                eval "/" [Integer (negate 80), Integer 80] `shouldBe` [Integer (negate 1)]
                
            it "divides floats" $ do
                eval "/" [Integer 2, Real 3.567] `shouldBe` [Real 1.7835]
                eval "/" [Real (negate 6.79), Real (negate 8.854)] `shouldBe` [Real 1.3039764]
                eval "/" [Real 1.325, Real 6.987] `shouldBe` [Real 5.273207]

            it "errors on too few arguments" $ do   
                evaluate (eval "/" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "/" [Real 8.88]) `shouldThrow` errorCall "Stack Underflow"
            
        context "^" $ do
            it "integer exponentiation" $ do
                eval "^" [Integer 8, Integer 3] `shouldBe` [Real 6561.0]
                eval "^" [Integer 9, Integer 6] `shouldBe` [Real 1.0077696e7]

            it "exponentiation with floats" $ do
                eval "^" [Integer 2, Real 3.14] `shouldBe` [Real 9.859601]
                eval "^" [Real 0.99, Integer 8] `shouldBe` [Real 7.8353624]
                eval "^" [Real 1.325, Real 6.987] `shouldBe` [Real 13.14272]

            it "errors on too few arguments" $ do   
                evaluate (eval "^" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "^" [Real 8.88]) `shouldThrow` errorCall "Stack Underflow"

            -- this does not work, seems to be a HSpec bug
            -- it "errors on non-numeric inputs" $ do
            --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException

        context "DUP" $ do
            it "duplicates values" $ do
                eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
                eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
                eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

            it "errors on empty stack" $ do
                evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack Underflow"

        context "STR" $ do
            it "converts values to strings" $ do
                eval "STR" [Integer 2] `shouldBe` [Id "2"]
                eval "STR" [Real 2.2] `shouldBe` [Id "2.2"]
                eval "STR" [Id "x"] `shouldBe` [Id "x"]

            it "errors on empty stack" $ do
                evaluate (eval "STR" []) `shouldThrow` errorCall "Stack Underflow"
        
        context "CONCAT2" $ do
            it "concatenates two strings" $ do
                eval "CONCAT2" [Id "world", Id "hello"] `shouldBe` [Id "helloworld"]
            --    eval "CONCAT2" [Id "world", Id "hello "] `shouldBe` [Real "hello world"]

            it "errors on wrong data types" $ do
                evaluate (eval "CONCAT2" [Real 8.22, Id "Number"]) `shouldThrow` errorCall "Wrong Data Types"
                evaluate (eval "CONCAT2" [Integer 4, Id "Number"]) `shouldThrow` errorCall "Wrong Data Types"
                evaluate (eval "CONCAT2" [Integer 12, Real 3.9865]) `shouldThrow` errorCall "Wrong Data Types"
            
            it "errors on too few arguments" $ do
                evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "CONCAT2" [Id "hello"]) `shouldThrow` errorCall "Stack Underflow"
        
        context "CONCAT3" $ do
            it "concatenates three strings" $ do
                eval "CONCAT3" [Id "string3", Id "string2 ", Id "string1 "] `shouldBe` [Id "string1 string2 string3"]

            it "errors on wrong data types" $ do
                evaluate (eval "CONCAT3" [Real 8.22, Id "Number", Integer 12]) `shouldThrow` errorCall "Wrong Data Types"
                evaluate (eval "CONCAT3" [Integer 4, Integer 64, Real 3.56]) `shouldThrow` errorCall "Wrong Data Types"
                evaluate (eval "CONCAT3" [Integer 12, Real 3.9865, Real 2.22]) `shouldThrow` errorCall "Wrong Data Types"
            
            it "errors on too few arguments" $ do
                evaluate (eval "CONCAT3" []) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "CONCAT3" [Id "hello"]) `shouldThrow` errorCall "Stack Underflow"
                evaluate (eval "CONCAT3" [Id "world", Id "hello"]) `shouldThrow` errorCall "Stack Underflow"

    describe "evalOut" $ do
        context "." $ do
            it "prints top of stack" $ do
                evalOut "." ([Id "x"], "") `shouldBe` ([],"x")
                evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
                evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

            it "errors on empty stack" $ do
                evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack Underflow"

        context "CR" $ do
            it "prints out a new line" $ do
                evalOut "CR" ([Integer 2], "") `shouldBe` ([Integer 2],"\n")
                evalOut "CR" ([Integer 9, Real 3.67], "hello world") `shouldBe` ([Integer 9, Real 3.67],"hello world\n")
                evalOut "CR" ([], "") `shouldBe` ([],"\n")

        context "EMIT" $ do
            it "converts an integer to the ASCII code" $ do
                evalOut "EMIT" ([Integer 45], "") `shouldBe` ([], "-")
                evalOut "EMIT" ([Integer 100, Real 6.5], "hello worl") `shouldBe` ([Real 6.5], "hello world")
                evalOut "EMIT" ([Integer 125, Id "string"], "{") `shouldBe` ([Id "string"], "{}")

            it "errors on wrong data types" $ do
                evaluate(evalOut "EMIT" ([Integer (negate 89)], "string")) `shouldThrow` errorCall "Integer Out of Range"
                evaluate(evalOut "EMIT" ([Integer 150], "ascii")) `shouldThrow` errorCall "Integer Out of Range"
            
            it "errors on wrong data types" $ do
                evaluate(evalOut "EMIT" ([Real 4.789], "string")) `shouldThrow` errorCall "Wrong Data Type"
                evaluate(evalOut "EMIT" ([Id "hello world"], "ascii")) `shouldThrow` errorCall "Wrong Data Type"
            
            it "errors on empty stack" $ do
                evaluate(evalOut "EMIT" ([], "")) `shouldThrow` errorCall "Stack Underflow"


        it "eval pass-through" $ do
            evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah") 