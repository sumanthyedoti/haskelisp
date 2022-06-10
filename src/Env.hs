module Env
  ( env
  , eval
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
  , ("=", numBoolOp (==))
  , (">", numBoolOp (>))
  , ("<=", numBoolOp (<=))
  , ("<", numBoolOp (<))
  , (">=", numBoolOp (>=))
  , ("all", boolOp (&&))
  , ("any", boolOp (||))
  , ("number?", isNumber)
  , ("string?", isString)
  , ("if", conditional)
  -- , ("atom?", isAtom)
  ]

numericOp :: String -> (Double -> Double -> Double) -> [LispVal] -> LispVal
numericOp "+" _ [] = Number 0
numericOp "*" _ [] = Number 1
numericOp atom _ [] = Error $ "No arguments passed to '" ++ atom ++ "'"
numericOp _ op params = Number $ foldl1 op $ map readNum params

numBoolOp :: (Double -> Double -> Bool) -> [LispVal] -> LispVal
numBoolOp op params =
  let left = readNum $ params !! 0
      right = readNum $ params !! 1
   in Bool $ op left right

boolOp :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
boolOp op params = Bool $ foldl1 op $ map readBool params

readNum :: LispVal -> Double
readNum (Number n) = n

readBool :: LispVal -> Bool
readBool (Bool n) = n

conditional :: [LispVal] -> LispVal
conditional [cond, conseq, alt] =
  case eval (cond) of
    Bool True -> eval (conseq)
    Bool False -> eval (alt)
    _ -> Error "at evaluation of test condition of 'if'"
conditional args =
  Error $
  (if (length args > 3)
     then "Too many"
     else "Too few") ++
  " arguments passed 'if'"

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args
eval (Error message) = LispVal.Error $ "Error: " ++ message
eval _ = LispVal.Error "Can not parse"

apply :: String -> [LispVal] -> LispVal
apply func args =
  maybe (LispVal.Error $ func ++ " can not be found") ($ args) $
  (lookup func env)
