module LispVal where

import Control.Monad.Except

data LispError
  = NumArgs String Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser String
  | BadSpecialForm String String
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either LispError

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
  show (Parser parseErr) = "Parse error at " ++ show parseErr

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Double
  | String String
  | Bool Bool
  | Error String

instance Show LispVal where
  show (Atom atom) = atom
  show (String str) = str
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List ls) = "(" ++ unwordList ls ++ ")"
  show (DottedList head tail) =
    "(" ++ unwordList head ++ " . " ++ show tail ++ ")"
  show (Error str) = "Error: " ++ str

unwordList :: [LispVal] -> String
unwordList = unwords . map show
