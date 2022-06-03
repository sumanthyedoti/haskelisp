module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec as P hiding (spaces)

import Lib

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

instance Show LispVal where
  show (Atom atom) = "A: " ++ atom
  show (String str) = "S: " ++ str
  show (Number contents) = "N: " ++ show contents
  show (Bool True) = "B: " ++ "#t"
  show (Bool False) = "B: " ++ "#f"
  show (List ls) = "L: " ++ "(" ++ unwordList ls ++ ")"
  show (DottedList head tail) =
    "D: " ++
    "h: " ++ "(" ++ unwordList head ++ " . " ++ "t: " ++ show tail ++ ")"

unwordList :: [LispVal] -> String
unwordList = unwords . map show

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces0 :: Parser ()
spaces0 = P.skipMany P.space

spaces1 :: Parser ()
spaces1 = P.skipMany1 P.space

parseString :: Parser LispVal
parseString = do
  P.char '"'
  x <- P.many (P.noneOf ['"'])
  P.char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- P.letter <|> symbol
  rest <- many (P.letter <|> P.digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  num <- P.many1 P.digit
  return $ (Number . read) num

parseList :: Parser LispVal
parseList = liftM List $ P.sepBy1 parseExpr spaces1

parseDottedList = do
  head <- endBy parseExpr spaces1
  tail <- P.char '.' >> spaces1 >> parseExpr
  return $ DottedList head tail

parseQuote = do
  P.char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

{-|
  Note that by referring to parseExpr within our parsers,
  we can nest them arbitrarily deep.
-}
parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuote <|> do
    char '('
    x <- P.try parseList <|> parseDottedList
    char ')'
    return x

readExpr :: String -> String
readExpr input =
  case P.parse (spaces0 >> parseExpr) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
