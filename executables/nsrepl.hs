module Main where

import NScriptParser.Parser
import NScriptParser.ParserMonad
import NScriptParser.AST
import NScriptInterpreter.Value
import NScriptInterpreter.InterpreterMonad
import NScriptInterpreter.Interpreter

import Data.Char (isSpace, isAlpha, isDigit)
import Control.Monad.Trans
import Control.Monad.State
import System.Console.Haskeline

type ReplMonadT = StateT Env (InputT IO)

{-|This is the main function of the interpreter-}
main :: IO()
main = runInputT defaultSettings $ evalStateT loop emptyEnv
	where
    loop :: ReplMonadT ()
    loop = do
      minput <- lift $ getInputLine "NjuScript> "
      case minput of
        Nothing -> lift $ outputStrLn "Bye-bye!"
        Just input -> 
          do
            let line = unwords $ words input
            case line of
              "" -> loop
              _ ->
                do
                  case runP parser line 0 of
                    FailedP errMsg ->
                      liftIO $ putStrLn $ "[Parser error] " ++ errMsg
                    OkP prog ->
                      do
                        env <- get
                        case evalProg prog env of
                          (Left err, _) -> 
                            liftIO $ putStrLn $ "[Evaluation error] " ++ show err
                          (Right v, newEnv) ->
                            do
                              liftIO $ putStrLn $ show v
                              put newEnv
                  loop