module Errors where

import Control.Monad.Except

import LispVal

data Args
  = Atleast
  | Exact

data LispError
  = NumArgs Args String Integer [LispVal]
  | TypeMismatch String LispVal
  | ParserError String
  | BadSpecialForm String String
  | NotFunction String
  | UnboundVar String String
  | Default String
  | Unexpected String

instance Show LispError where
  show (NumArgs args funcName expected found) =
    funcName ++
    "Expectes " ++
    (case args of
       Atleast -> "atleast "
       Exact -> "") ++
    show expected ++
    " args; found" ++ (show $ length found) ++ "values, " ++ unwordList found
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction func) = "Function'" ++ show func ++ "' not found"
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (ParserError parseErr) = "Error parsing " ++ show parseErr
  show (Unexpected err) = "Unexpected " ++ show err

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
