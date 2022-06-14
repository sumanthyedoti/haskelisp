module LispVal where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Double
  | String String
  | Bool Bool

instance Show LispVal where
  show (Atom atom) = atom
  show (String str) = str
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List ls) = "(" ++ unwordList ls ++ ")"
  show (DottedList head tail) =
    "(" ++ unwordList head ++ " . " ++ show tail ++ ")"

unwordList :: [LispVal] -> String
unwordList = unwords . map show
