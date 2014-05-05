module NScriptInterpreter.Interpreter (exec) where

import NScriptParser.AST
import qualified Data.Map as Map

data Value
  = IntVal Int
  | StrVal String
  | CloVal String Exp Env

instance Show Value where
  show (IntVal i) = show i
  show (StrVal str) = "\"" ++ str ++ "\""
  show (CloVal argName _ _) = "<closure with arg " ++ argName ++ ">"

type Env = Map.Map String Value
type EvalResult = Either String Value

emptyEnv :: Env
emptyEnv = Map.empty

insertEnv :: Env -> String -> Value -> Env
insertEnv oldEnv newId newVal = Map.insert newId newVal oldEnv

lookupEnv :: Env -> String -> Maybe Value
lookupEnv env id = Map.lookup id env

valuePlus :: Value -> Value -> Maybe Value
valuePlus (IntVal v1) (IntVal v2) = Just $ IntVal $ v1 + v2
valuePlus (StrVal v1) (StrVal v2) = Just $ StrVal $ v1 ++ v2
valuePlus _ _ = Nothing

valueMinus :: Value -> Value -> Maybe Value
valueMinus (IntVal v1) (IntVal v2) = Just $ IntVal $ v1 - v2
valueMinus _ _ = Nothing

valueMul :: Value -> Value -> Maybe Value
valueMul (IntVal v1) (IntVal v2) = Just $ IntVal $ v1 * v2
valueMul _ _ = Nothing

valueDiv :: Value -> Value -> Maybe Value
valueDiv (IntVal v1) (IntVal v2) = Just $ IntVal $ v1 `div` v2
valueDiv _ _ = Nothing

evalBinaryExp :: (Value -> Value -> Maybe Value) -> Exp -> Exp -> Env -> EvalResult
evalBinaryExp f e1 e2 env = 
  case eval e1 env of
    Left errMsg -> Left errMsg
    Right v1 ->
      case eval e2 env of
        Left errMsg -> Left errMsg
        Right v2 ->
          case f v1 v2 of
            Nothing -> Left $ "Type error when performing computation on " ++ show v1 ++ " and " ++ show v2
            Just v -> Right v

eval :: Exp -> Env -> EvalResult
eval (IntLit i) env = Right (IntVal i)
eval (StrLit str) env = Right (StrVal str)
eval (Var id) env =
  case lookupEnv env id of
    Nothing -> Left $ "Unbound variable: " ++ id
    Just v -> Right v
eval (Plus e1 e2) env = evalBinaryExp valuePlus e1 e2 env
eval (Minus e1 e2) env = evalBinaryExp valueMinus e1 e2 env
eval (Mul e1 e2) env = evalBinaryExp valueMul e1 e2 env
eval (Div e1 e2) env = evalBinaryExp valueDiv e1 e2 env
eval (Let id exp body) env = 
  case eval exp env of
    Left errMsg -> Left errMsg
    Right idV ->
      eval body (insertEnv env id idV)
eval (Lambda id exp) env = Right $ CloVal id exp env
eval (FunApp funExp argExp) env =
  case eval funExp env of
    Left errMsg -> Left errMsg
    Right funV ->
      case funV of
        CloVal arg body oldEnv ->
          case eval argExp env of
            Left errMsg -> Left errMsg
            Right argV ->
              eval body (insertEnv oldEnv arg argV)
        _ -> Left $ "Trying to use value " ++ show funV ++ " as a function!"
  
exec :: Exp -> EvalResult
exec exp = eval exp emptyEnv

