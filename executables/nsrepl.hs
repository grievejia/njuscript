module Main where

import NScriptParser.Parser
import NScriptParser.ParserMonad
import NScriptParser.AST
import NScriptInterpreter.Interpreter

import Data.Char (isSpace, isAlpha, isDigit)
import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> Env -> IO Env
process line env =
  case runP parser line 0 of
    FailedP errMsg ->
      do
       putStrLn $ "[Parser error] " ++ errMsg
       return env
    OkP (ExpStmt exp) ->
      case eval exp env of
        Left errMsg -> 
          do
            putStrLn $ "[Evaluation error] " ++ errMsg
            return env
        Right v ->
          do
            putStrLn $ show v
            return env
    OkP (DefStmt id exp) ->
      case eval exp env of
        Left errMsg ->
          do
            putStrLn $ "[Evaluation error] " ++ errMsg
            return env
        Right v ->
          do
            putStrLn $ id ++ " := " ++ show v
            return $ insertEnv env id v
  

{-|This is the main function of the interpreter-}
main :: IO()
main = runInputT defaultSettings (loop emptyEnv)
	where
   loop env = do
    minput <- getInputLine "NjuScript> "
    case minput of
      Nothing -> outputStrLn "Bye-bye!"
      Just input -> (liftIO $ process input env) >>= loop