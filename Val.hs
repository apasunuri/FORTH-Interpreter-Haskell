module Val where
-- this file contains definitions for Val and aux functions

import Data.Maybe (isJust)
import Text.Read (readMaybe)

-- The values manipulated by FORTH
data Val = Integer Int 
    | Real Float
    | Id String
    deriving (Show, Eq)

-- converts string to Val 
-- sequence tried is Integer, Float, String
strToVal :: String -> Val
strToVal s = case readMaybe s :: Maybe Int of
    Just i -> Integer i
    Nothing -> case readMaybe s :: Maybe Float of
        Just f -> Real f 
        Nothing -> Id s

-- converts to Float if Real or Integer, error otherwise
-- used to deal with arguments of operators
toFloat :: Val -> Float
toFloat (Real x) = x
toFloat (Integer i) = fromIntegral i     
toFloat (Id _) = error "Not convertible to float"