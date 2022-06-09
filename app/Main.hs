module Main where

import System.Environment

import Eval
import Parser

main :: IO ()
main = do
  (expr:_) <- getArgs
  print $ eval $ parse expr
