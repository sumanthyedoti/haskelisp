module LispVal where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
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
