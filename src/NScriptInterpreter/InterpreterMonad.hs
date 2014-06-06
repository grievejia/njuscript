module NScriptInterpreter.InterpreterMonad (EvalError(..), EvalMonad, EnvMonad, runReaderT, ask, local, get, put, throwError) where

{-|
 This module contains all the monads and monad transformers that will be used by the interpreter
-}

import NScriptInterpreter.Value
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

data EvalError
  -- | Divide by zero error
  = DivByZero
  -- | The error message saying that two values have mismatched types
	| TypeMismatch Value Value
  -- | The error message saying that a given variable is unbound
  | UnboundVariable String
  -- | Trying to apply a non-closure value as a function
  | ApplyNonClosure Value
  -- | Trying to use a non-bool value as a if condition
  | ConditionNotBool Value
  -- | Other unspecified errors
  | OtherError String

-- | We make LengthError an instance of the Error class to be able to throw it as an exception.
instance Error EvalError where
  noMsg    = OtherError "Unknown error"
  strMsg s = OtherError s

-- | Converts EvalError to a readable message.
instance Show EvalError where
  show DivByZero = "Divide by zero"
  show (TypeMismatch v1 v2) =
      "Type error when performing computation on " ++ show v1 ++ " and " ++ show v2
  show (UnboundVariable varName) = "Unbound variable: " ++ varName
  show (ApplyNonClosure v) = "Trying to use value " ++ show v ++ " as a function!"
  show (ConditionNotBool v) = "Trying to use a non-bool value " ++ show v ++ " as an if condition!"
  show (OtherError msg) = msg

-- | For our monad type constructor, we use Either EvalError which represents failure using Left or a successful result of type a using Right.
type EvalMonad = Either EvalError

-- | This is a monad that encapsulate the environment
type EnvMonad = ReaderT Env EvalMonad
type ReplMonad = StateT Env EvalMonad
