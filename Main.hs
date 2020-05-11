module Main where

-- Running: runaskell Main.hs path_to_test_file

import Interpret
import System.Environment

main :: IO ()
main = do
    (fileName:tl) <- getArgs
    contents <- readFile fileName
    let (stack, output) = interpret contents
    if length stack /= 0
        then do 
            print("Stack is not Empty")
            putStrLn(show stack)   
    else putStrLn output
