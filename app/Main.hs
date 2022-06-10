module Main where

import System.Environment

import Eval
import Parser
import System.IO

main :: IO ()
main = do
  hSetEncoding stdin utf8
  (expr:_) <- getArgs
  print $ eval $ parse expr
