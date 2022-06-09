module Eval
  ( eval
  ) where

import Env
import LispVal

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
