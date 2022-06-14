{-# LANGUAGE LambdaCase #-}

module Errors where

import Control.Monad.Except

import LispVal

data Args
  = Atleast
  | Exact

type Function = String

data LispError
  = NumArgs Args Function Int [LispVal]
  | TypeMismatch Function String LispVal
  | ParserError String
  | BadSpecialForm String String
  | NotFunction String
  | UnboundVar String String
  | Default String
  | Unexpected String

pluralize :: Int -> String -> String
pluralize num word =
  if num == 1
    then ' ' : word
    else ' ' : word ++ "s"

showType :: LispVal -> String
showType =
  \case
    List _ -> "list"
    String _ -> "string"
    Atom _ -> "atom"
    Bool _ -> "boolean"
    Number _ -> "number"

instance Show LispError where
  show (NumArgs args funcName expected found) =
    '\'' :
    funcName ++
    "' expectes " ++
    (case args of
       Atleast -> "atleast "
       Exact -> "") ++
    show expected ++
    pluralize expected "arg" ++
    "; found " ++
    (show $ length found) ++
    pluralize (length found) "value" ++ ", " ++ unwordList found
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction func) = "Function'" ++ show func ++ "' not found"
  show (TypeMismatch func expected found) =
    "Invalid type: " ++
    func ++ " expected " ++ expected ++ ", found " ++ showType found
  show (ParserError parseErr) = "Error parsing " ++ show parseErr
  show (Unexpected err) = "Unexpected " ++ show err
  show (Default err) = "Error: " ++ show err

type ThrowsError = Either LispError

-- will always have valid (Right) data
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
