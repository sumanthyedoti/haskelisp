module Parser
  ( eval
  , Parser.parse
  , LispVal
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec as P hiding (spaces)

import Env
import LispVal

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces0 :: P.Parser ()
spaces0 = P.skipMany P.space

spaces1 :: P.Parser ()
spaces1 = P.skipMany1 P.space

parseString :: P.Parser LispVal
parseString = do
  P.char '"'
  x <- P.many (P.noneOf ['"']) -- ! implement escape characters
  P.char '"'
  return $ String x

parseAtom :: P.Parser LispVal
parseAtom = do
  first <- P.letter <|> symbol
  rest <- many (P.letter <|> P.digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseNumber :: P.Parser LispVal
parseNumber = do
  num <- P.many1 P.digit
  return $ (Number . read) num

parseList :: P.Parser LispVal
parseList = liftM List $ P.sepBy1 parseExpr spaces1

parseDottedList = do
  head <- endBy parseExpr spaces1
  tail <- P.char '.' >> spaces1 >> parseExpr
  return $ DottedList head tail

parseQuote = do
  P.char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: P.Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuote <|> do
    P.char '('
    x <- P.try parseList <|> parseDottedList
    P.char ')'
    return x

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args
eval _ = Error "Can not parse"

apply :: String -> [LispVal] -> LispVal
apply func args =
  maybe (Error $ func ++ " can not be found") ($ args) $ (lookup func env)

parse :: String -> LispVal
parse input =
  case P.parse (spaces0 >> parseExpr) "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val
