module Interpret where
-- this file contains the FORTH interpreter

import Val
import Eval
import Flow

-- inner function for foldl
-- Takes the current stack and an input and 
-- computes the next stack
evalF :: ([Val], String) -> Val -> ([Val], String)
evalF s (Id op) = evalOut op s
-- cannot run, put on the stack and preserve output
evalF (s, out) x = (x:s,out)

-- function to interpret a string into a stack and 
-- an output string
interpret :: String -> ([Val], String)
interpret text = text |>
    words |> -- brake text into words
    map strToVal |> -- strings to instructions
    foldl evalF ([], "") -- perform evaluation
