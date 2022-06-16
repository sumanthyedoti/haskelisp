module Main where

import Control.Monad
import System.Environment
import System.IO

import Env
import Errors
import Eval
import Parser

readExpr :: IO String
readExpr = putStr "hlisp> " >> hFlush stdout >> getLine

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
  evaled <- runIOThrows $ liftM show ((liftThrows $ parse expr) >>= eval env)
  putStrLn $ evaled

runRepl_ :: Env -> IO ()
runRepl_ env = do
  input <- readExpr
  if (input == ":quit" || input == ":q")
    then return ()
    else evalAndPrint env input >> runRepl_ env

--
main :: IO ()
main = nullEnv >>= runRepl_
