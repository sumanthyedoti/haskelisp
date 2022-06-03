module Env
  ( env
  ) where

import LispVal

env :: [(String, [LispVal] -> LispVal)]
env =
  [ ("+", numericOp (+))
  , ("-", numericOp (-))
  , ("*", numericOp (*))
  , ("/", numericOp div)
  , ("mod", numericOp mod)
  ]

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericOp op params = Number $ foldl1 op $ map readNum params

readNum :: LispVal -> Integer
readNum (Number n) = n
