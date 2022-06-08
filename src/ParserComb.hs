{-# LANGUAGE LambdaCase #-}

module ParserComb where

import Control.Applicative
import qualified Data.Char as C

data Error
  = Unexpected String
  | EndOfInput
  | CustomError String
  deriving (Show)

newtype Parser v =
  Parser
    { parse :: String -> Either Error (v, String)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (v, rest) <- p input
      return (f v, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (f, input') <- p1 input
      (v, input'') <- p2 input'
      return (f v, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left $ CustomError ""
  (Parser p1) <|> (Parser p2) =
    Parser $ \input ->
      case p1 input of
        Right (v1, rest1) -> return (v1, rest1)
        _ ->
          case p2 input of
            Right (v2, rest2) -> return (v2, rest2)
            _ -> Left $ Unexpected (take 1 input)

spanTill :: (Char -> Bool) -> Parser String
spanTill f = Parser $ Right . span f

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate =
  Parser $ \case
    [] -> Left EndOfInput
    (hd:rest)
      | predicate hd -> Right (hd, rest)
      | otherwise -> Left $ Unexpected ("'" ++ [hd] ++ "'")

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

ws :: Parser String
ws = spanTill C.isSpace
