module LispVal where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Double
  | String String
  | Bool Bool
  | Error String

instance Show LispVal where
  show (Atom atom) = "A " ++ atom
  show (String str) = "S " ++ str
  show (Number contents) = "N " ++ show contents
  show (Bool True) = "B " ++ "#t"
  show (Bool False) = "B " ++ "#f"
  show (List ls) = "L (" ++ unwordList ls ++ ")"
  show (DottedList head tail) =
    "DL (" ++ unwordList head ++ " . " ++ show tail ++ ")"
  show (Error str) = "Error: " ++ str

unwordList :: [LispVal] -> String
unwordList = unwords . map show
