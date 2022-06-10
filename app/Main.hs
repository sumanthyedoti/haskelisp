module Main where

import System.Environment

import Env (eval)
import Parser
import System.IO

main :: IO ()
main = do
  hSetEncoding stdin utf8
  (expr:_) <- getArgs
  print $ eval $ parse expr
