{-# LANGUAGE LambdaCase #-}

module Env where

import Control.Monad.Except
import Data.IORef
import Data.Maybe

{-| Data Types
   ============
-}
{-| Errors
   --------
-}
data Args
  = Atleast
  | Exact

data UnboundType
  = Get
  | Set

type Function = String

type Variable = String

data LispError
  = NumArgs Args Function Int [LispVal]
  | TypeMismatch Function String LispVal
  | ParserError String
  | BadSpecialForm String String
  | NotFunction String
  | UnboundVar UnboundType Variable
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
    show (length found) ++
    pluralize (length found) "value" ++ ", " ++ unwordList found
  show (UnboundVar action varname) =
    (case action of
       Get -> "Getting"
       Set -> "Setting") ++
    " an unbound varibale " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction func) = "Function'" ++ show func ++ "' not found"
  show (TypeMismatch func expected found) =
    "Invalid type: '" ++
    func ++ "' expects " ++ expected ++ ", found " ++ showType found
  show (ParserError parseErr) = "Error parsing " ++ show parseErr
  show (Unexpected err) = "Unexpected " ++ show err
  show (Default err) = "Error: " ++ show err

type ThrowsError = Either LispError

{-| Lisp Values
   -------------
-}
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Double
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String]
      , body :: [LispVal]
      , closure :: Env
      }

instance Show LispVal where
  show (Atom atom) = atom
  show (String str) = "\"" ++ str ++ "\""
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List ls) = "(" ++ unwordList ls ++ ")"
  show (DottedList head tail) =
    "(" ++ unwordList head ++ " . " ++ show tail ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params = args, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ ")...)"

unwordList :: [LispVal] -> String
unwordList = unwords . map show

{-| Env
   ------
-}
type Env = IORef [(String, IORef LispVal)]

-- monad transformer: that may contain IO actions that throw a LispError
type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- catches Error and will return valid (Right) data
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
  runExceptT (catchError action (return . show)) >>=
  return . (\(Right val) -> val)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar Get var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef varName val = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar Set varName)
    (liftIO . (`writeIORef` val))
    (lookup varName env)
  return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef varName val = do
  alreadyDefined <- liftIO $ isBound envRef varName
  if alreadyDefined
    then setVar envRef varName val >> return val
    else liftIO $ do
           valueRef <- newIORef val
           env <- readIORef envRef
           writeIORef envRef ((varName, valueRef) : env)
           return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = (++ env) <$> mapM addBinding bindings
    addBinding (var, val) = do
      ref <- newIORef val
      return (var, ref)
