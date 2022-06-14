module Errors
  ( LispError
  , ThrowsError
  ) where

import Control.Monad.Except

import LispVal

data LispError
  = NumArgs String Integer [LispVal]
  | TypeMismatch String LispVal
  | ParserError String
  | BadSpecialForm String String
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show (NumArgs funcName expected found) =
    funcName ++
    "Expectes " ++
    show expected ++
    " args; found" ++ (show $ length found) ++ "values, " ++ unwordList found
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (ParserError parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
