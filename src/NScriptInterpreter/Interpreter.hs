module NScriptInterpreter.Interpreter (Env, emptyEnv, insertEnv, exec, eval) where

import NScriptParser.AST
import qualified Data.Map as Map

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

valueLt :: Value -> Value -> Maybe Value
valueLt (IntVal v1) (IntVal v2) = Just $ BoolVal $ v1 < v2
valueLt (StrVal v1) (StrVal v2) = Just $ BoolVal $ v1 < v2
valueLt _ _ = Nothing

valueLe :: Value -> Value -> Maybe Value
valueLe (IntVal v1) (IntVal v2) = Just $ BoolVal $ v1 <= v2
valueLe (StrVal v1) (StrVal v2) = Just $ BoolVal $ v1 <= v2
valueLe _ _ = Nothing

valueGt :: Value -> Value -> Maybe Value
valueGt (IntVal v1) (IntVal v2) = Just $ BoolVal $ v1 > v2
valueGt (StrVal v1) (StrVal v2) = Just $ BoolVal $ v1 > v2
valueGt _ _ = Nothing

valueGe :: Value -> Value -> Maybe Value
valueGe (IntVal v1) (IntVal v2) = Just $ BoolVal $ v1 >= v2
valueGe (StrVal v1) (StrVal v2) = Just $ BoolVal $ v1 >= v2
valueGe _ _ = Nothing

valueEq :: Value -> Value -> Maybe Value
valueEq (IntVal v1) (IntVal v2) = Just $ BoolVal $ v1 == v2
valueEq (StrVal v1) (StrVal v2) = Just $ BoolVal $ v1 == v2
valueEq _ _ = Just $ BoolVal False

valueNe :: Value -> Value -> Maybe Value
valueNe (IntVal v1) (IntVal v2) = Just $ BoolVal $ v1 /= v2
valueNe (StrVal v1) (StrVal v2) = Just $ BoolVal $ v1 /= v2
valueNe _ _ = Just $ BoolVal True

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
eval (BoolLit b) env = Right (BoolVal b)
eval (StrLit str) env = Right (StrVal str)
eval (Var id) env =
  case lookupEnv env id of
    Nothing -> Left $ "Unbound variable: " ++ id
    Just v -> Right v
eval (Arith Plus e1 e2) env = evalBinaryExp valuePlus e1 e2 env
eval (Arith Minus e1 e2) env = evalBinaryExp valueMinus e1 e2 env
eval (Arith Mul e1 e2) env = evalBinaryExp valueMul e1 e2 env
eval (Arith Div e1 e2) env = evalBinaryExp valueDiv e1 e2 env
eval (Compare Less e1 e2) env = evalBinaryExp valueLt e1 e2 env
eval (Compare LessEq e1 e2) env = evalBinaryExp valueLe e1 e2 env
eval (Compare Greater e1 e2) env = evalBinaryExp valueGt e1 e2 env
eval (Compare GreaterEq e1 e2) env = evalBinaryExp valueGe e1 e2 env
eval (Compare Equal e1 e2) env = evalBinaryExp valueEq e1 e2 env
eval (Compare NotEqual e1 e2) env = evalBinaryExp valueNe e1 e2 env
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
eval (If condExp thenExp elseExp) env =
  case eval condExp env of
    Left errMsg -> Left errMsg
    Right condV ->
      case condV of
        BoolVal True -> eval thenExp env
        BoolVal False -> eval elseExp env
        _ -> Left $ "Trying to use a non-bool value " ++ show condV ++ " as an if condition!"
  
exec :: Exp -> EvalResult
exec exp = eval exp emptyEnv

