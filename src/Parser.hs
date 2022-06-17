module Parser
  ( Parser.parse
  , LispVal
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.List
import Data.Maybe

import ParserLib

import Eval

import Env

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces0 :: Parser String
spaces0 = skipMany space

spaces1 :: Parser String
spaces1 = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ satisfy (/= '"')
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  num <- number
  return $ Number num

parseList :: Parser LispVal
parseList = List <$> (char '(' *> sepBy spaces1 parseExpr <* char ')')

parseQuote :: Parser LispVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuote <|> parseList

parse :: String -> ThrowsError LispVal
parse input =
  case ParserLib.parse parseExpr input of
    Left err -> throwError $ ParserError err
    Right (val, "") -> return val
    Right (val, rest) -> throwError $ Unexpected rest
