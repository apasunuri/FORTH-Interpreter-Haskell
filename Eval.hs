module Eval where
-- This file contains definitions for functions and operators

import Val

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack Underflow")

eval "+" (Integer x: Integer y:tl) = Integer(x + y) : tl
eval "+" (x:y:tl) = (Real $ toFloat x + toFloat y) : tl
eval "+" _ = error("Stack Underflow")

eval "-" (Integer x: Integer y:tl) = Integer(y - x) : tl
eval "-" (x:y:tl) = (Real $ toFloat y - toFloat x) : tl
eval "-" _ = error("Stack Underflow")

eval "/" (Integer x: Integer y:tl) = Integer(y `div` x) : tl
eval "/" (x:y:tl) = (Real $ toFloat y / toFloat x) : tl
eval "/" _ = error("Stack Underflow")

eval "^" (x:y:tl) = (Real(toFloat y ** toFloat x)) : tl
eval "^" _ = error("Stack Underflow")

eval "STR" (Id x:tl) = Id(x) : tl
eval "STR" (Integer x:tl) = Id(show x) : tl
eval "STR" (Real x:tl) = Id(show x) : tl
eval "STR" _ = error("Stack Underflow")

eval "CONCAT2" (Id x: Id y:tl) = Id(y ++ x) : tl
eval "CONCAT2" (x:y:tl) = error("Wrong Data Types")
eval "CONCAT2" _ = error("Stack Underflow")

eval "CONCAT3" (Id x: Id y: Id z:tl) = Id(z ++ y ++ x) : tl
eval "CONCAT3" (x:y:z:tl) = error("Wrong Data Types")
eval "CONCAT3" _ = error("Stack Underflow")

-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack Underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "CR" (tl, out) = (tl, out ++ "\n")
--evalOut "EMIT" (Integer i:tl, out) = (tl, out ++ [toEnum i :: Char])
evalOut "EMIT" (Integer i:tl, out) = if(i `elem` [0..127]) then (tl, out ++ [toEnum i :: Char]) else error "Integer Out of Range"
evalOut "EMIT" (Real x:tl, out) = error "Wrong Data Type"
evalOut "EMIT" (Id x:tl, out) = error "Wrong Data Type"
evalOut "EMIT" ([], _) = error "Stack Underflow"
evalOut "." ([], _) = error "Stack Underflow"

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)