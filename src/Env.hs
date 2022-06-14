module Env
  ( env
  , eval
  ) where

import Control.Monad.Except
import Data.Fixed
import Data.Maybe

import Errors
import LispVal

env :: [(String, [LispVal] -> ThrowsError LispVal)]
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
  -- , ("if", conditional)
  -- , ("atom?", isAtom)
  ]

numericOp ::
     String -> (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
numericOp "+" _ [] = return $ Number 0
numericOp "*" _ [] = return $ Number 1
numericOp atom _ [] = throwError $ NumArgs Atleast atom 2 []
numericOp _ op params = mapM readNum params >>= return . Number . foldl1 op

numBoolOp :: (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolOp op params = do
  left <- readNum $ params !! 0
  right <- readNum $ params !! 1
  return $ Bool $ op left right

boolOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOp op params = mapM readBool params >>= return . Bool . foldl1 op

readNum :: LispVal -> ThrowsError Double
readNum (Number n) = return n

readBool :: LispVal -> ThrowsError Bool
readBool (Bool n) = return n

-- conditional :: [LispVal] -> LispVal
-- conditional [cond, conseq, alt] =
--   case eval (cond) of
--     Bool True -> eval (conseq)
--     Bool False -> eval (alt)
--     _ -> Error "at evaluation of test condition of 'if'"
-- conditional args =
--   Error $
--   (if (length args > 3)
--      then "Too many"
--      else "Too few") ++
--   " arguments passed 'if'"
isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [] = throwError $ NumArgs Exact "number?" 1 []
isNumber (x:_) =
  case x of
    Number _ -> return $ Bool True
    _ -> return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [] = throwError $ NumArgs Exact "string?" 1 []
isString (x:_) =
  case x of
    String _ -> return $ Bool True
    _ -> return $ Bool False

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ Unexpected "Can not parse"

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction $ show func) ($ args) $ (lookup func env)
