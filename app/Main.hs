module Main where

import System.Environment
import Text.ParserCombinators.Parsec as Parsec

import Lib

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!#$%&|*+-/:<=>?@^_~"

main :: IO ()
main = do
  someFunc
  putStr symbol
