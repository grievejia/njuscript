module NScriptInterpreter.Value where

{-|
 This module contains the definition of the Value used by our interpreter
-}

import NScriptParser.AST
import qualified Data.Map as Map

type Env = Map.Map String Value

emptyEnv :: Env
emptyEnv = Map.empty

insertEnv :: Env -> String -> Value -> Env
insertEnv oldEnv newId newVal = Map.insert newId newVal oldEnv

lookupEnv :: Env -> String -> Maybe Value
lookupEnv env id = Map.lookup id env

data Value
  = IntVal Integer
  | BoolVal Bool
  | StrVal String
  | CloVal String Exp Env

instance Show Value where
  show (IntVal i) = show i
  show (BoolVal b) = show b
  show (StrVal str) = "\"" ++ str ++ "\""
  show (CloVal argName _ _) = "<closure with arg " ++ argName ++ ">"
