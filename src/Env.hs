module Env
  ( env
  ) where

import Data.Fixed
import Data.Maybe
import LispVal

env :: [(String, [LispVal] -> LispVal)]
env =
  [ ("+", numericOp "+" (+))
  , ("-", numericOp "-" (-))
  , ("*", numericOp "*" (*))
  , ("/", numericOp "/" (/))
  , ("mod", numericOp "mod" mod')
  , ("number?", isNumber)
  , ("string?", isString)
  -- , ("atom?", isAtom)
  ]

numericOp :: String -> (Double -> Double -> Double) -> [LispVal] -> LispVal
numericOp "+" _ [] = Number 0
numericOp "*" _ [] = Number 1
numericOp atom _ [] = Error $ "No arguments passed to '" ++ atom ++ "'"
numericOp _ op params = Number $ foldl1 op $ map readNum params

readNum :: LispVal -> Double
readNum (Number n) = n

isNumber :: [LispVal] -> LispVal
isNumber [] = Error "No argument passed to 'number?'"
isNumber (x:_) =
  case x of
    Number _ -> Bool True
    _ -> Bool False

isString :: [LispVal] -> LispVal
isString [] = Error "No argument passed to 'string?'"
isString (x:_) =
  case x of
    String _ -> Bool True
    _ -> Bool False
