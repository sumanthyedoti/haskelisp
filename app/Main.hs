module Main where

import System.Environment

import Control.Monad
import Env (eval)
import Errors
import Parser

main :: IO ()
main = do
  (expr:_) <- getArgs
  evaled <- return $ liftM show (parse (expr) >>= eval)
  putStrLn $ extractValue $ trapError evaled
