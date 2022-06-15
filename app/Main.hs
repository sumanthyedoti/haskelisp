module Main where

import System.Environment

import Control.Monad
import Env (eval)
import Errors
import Parser
import System.IO

readExpr :: IO String
readExpr = putStr "hlisp> " >> hFlush stdout >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = do
  evaled <- return $ liftM show (parse expr >>= eval)
  putStrLn $ extractValue $ trapError evaled

runRepl_ :: IO ()
runRepl_ = do
  input <- readExpr
  if (input == ":quit" || input == ":q")
    then return ()
    else evalAndPrint input >> runRepl_

main :: IO ()
main = runRepl_
