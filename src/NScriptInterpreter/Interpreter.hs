module NScriptInterpreter.Interpreter (exec, evalProg) where

import NScriptParser.AST
import NScriptInterpreter.Value
import NScriptInterpreter.InterpreterMonad

valuePlus :: Value -> Value -> EnvMonad Value
valuePlus (IntVal v1) (IntVal v2) = return $ IntVal $ v1 + v2
valuePlus (StrVal v1) (StrVal v2) = return $ StrVal $ v1 ++ v2
valuePlus v1 v2 = throwError (TypeMismatch v1 v2)

valueDiv :: Value -> Value -> EnvMonad Value
valueDiv (IntVal v1) (IntVal 0) = throwError DivByZero
valueDiv (IntVal v1) (IntVal v2) = return $ IntVal $ v1 `div` v2
valueDiv v1 v2 = throwError (TypeMismatch v1 v2)

valueArith :: (Integer -> Integer -> Integer) -> Value -> Value -> EnvMonad Value
valueArith f (IntVal v1) (IntVal v2) = return $ IntVal (f v1 v2)
valueArith _ v1 v2 = throwError (TypeMismatch v1 v2)

valueMinus = valueArith (-)
valueMul = valueArith (*)

valueCmp :: (Integer -> Integer -> Bool) -> (String -> String -> Bool) -> Value -> Value -> EnvMonad Value
valueCmp f g (IntVal v1) (IntVal v2) = return $ BoolVal $ (f v1 v2)
valueCmp f g (StrVal v1) (StrVal v2) = return $ BoolVal $ (g v1 v2)
valueCmp _ _ v1 v2 = throwError (TypeMismatch v1 v2)

valueLt = valueCmp (<) (<)
valueLe = valueCmp (<=) (<=)
valueGt = valueCmp (>) (>)
valueGe = valueCmp (>=) (>=)
valueEq = valueCmp (==) (==)
valueNe = valueCmp (/=) (/=)

evalBinaryExp :: (Value -> Value -> EnvMonad Value) -> Exp -> Exp -> EnvMonad Value
evalBinaryExp f e1 e2 = 
  do
    v1 <- eval e1
    v2 <- eval e2
    f v1 v2

eval :: Exp -> EnvMonad Value
eval (IntLit i) = return $ IntVal i
eval (BoolLit b) = return $ BoolVal b
eval (StrLit str) = return $ StrVal str
eval (Var varId) =
  do
    env <- ask
    case lookupEnv env varId of
      Nothing -> throwError (UnboundVariable varId)
      Just v -> return v
eval (Arith Plus e1 e2) = evalBinaryExp valuePlus e1 e2
eval (Arith Minus e1 e2) = evalBinaryExp valueMinus e1 e2
eval (Arith Mul e1 e2) = evalBinaryExp valueMul e1 e2
eval (Arith Div e1 e2) = evalBinaryExp valueDiv e1 e2
eval (Compare Less e1 e2) = evalBinaryExp valueLt e1 e2
eval (Compare LessEq e1 e2) = evalBinaryExp valueLe e1 e2
eval (Compare Greater e1 e2) = evalBinaryExp valueGt e1 e2
eval (Compare GreaterEq e1 e2) = evalBinaryExp valueGe e1 e2
eval (Compare Equal e1 e2) = evalBinaryExp valueEq e1 e2
eval (Compare NotEqual e1 e2) = evalBinaryExp valueNe e1 e2
eval (Let varId varExp body) =
  do
    varVal <- eval varExp
    local (\oldEnv -> insertEnv oldEnv varId varVal) $ eval body
eval (Lambda argId body) = 
  do
    env <- ask
    return $ CloVal argId body env
eval (FunApp funExp argExp) =
  do
    funVal <- eval funExp
    argVal <- eval argExp
    case funVal of
      CloVal arg body oldEnv ->
        local (\_ -> insertEnv oldEnv arg argVal) $ eval body
      _ ->
        throwError (ApplyNonClosure funVal)
eval (If condExp thenExp elseExp) =
  do
    condVal <- eval condExp
    case condVal of
      BoolVal True -> eval thenExp
      BoolVal False -> eval elseExp
      _ -> throwError (ConditionNotBool condVal)

evalProg :: Program -> Env -> (EvalMonad Value, Env)
evalProg (DefStmt id exp) env = (val, newEnv)
  where
    val = runReaderT (eval exp) env
    newEnv =
      case val of
        Left _ -> env
        Right v -> insertEnv env id v

evalProg (ExpStmt exp) env = (runReaderT (eval exp) env, env)

  
exec :: Exp -> EvalMonad Value
exec exp = runReaderT (eval exp) emptyEnv

