{-# LANGUAGE LambdaCase #-}

module Env where

import Control.Monad.Except
import Data.IORef

{-| Data Types
   ============
-}
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

showVal (Atom atom) = atom
showVal (String str) = "\"" ++ str ++ "\""
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List ls) = "(" ++ unwordList ls ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++ ")...)"

instance Show LispVal where
  show = showVal

unwordList :: [LispVal] -> String
unwordList = unwords . map show

{-| Errors
   --------
-}
data Args
  = Atleast
  | Exact

type Function = String

type Variable = String

data LispError
  = NumArgs Args Function Int [LispVal]
  | TypeMismatch Function String LispVal
  | ParserError String
  | BadSpecialForm String String
  | NotFunction String
  | UnboundVar String Variable
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
  show (UnboundVar message varname) = message ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction func) = "Function'" ++ show func ++ "' not found"
  show (TypeMismatch func expected found) =
    "Invalid type: '" ++
    func ++ "' expects " ++ expected ++ ", found " ++ showType found
  show (ParserError parseErr) = "Error parsing " ++ show parseErr
  show (Unexpected err) = "Unexpected " ++ show err
  show (Default err) = "Error: " ++ show err

type ThrowsError = Either LispError

-- will always have valid (Right) data
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef varName val = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" varName)
    (liftIO . (flip writeIORef val))
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
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, val) = do
      ref <- newIORef val
      return (var, ref)
