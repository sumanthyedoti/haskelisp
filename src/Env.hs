module Env where

import Control.Monad.Except
import Data.IORef

import Errors
import LispVal

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
