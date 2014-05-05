module Main where

import NScriptParser.Parser
import NScriptParser.ParserMonad
import NScriptParser.AST
import NScriptInterpreter.Interpreter

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = runP parser line 0
  case res of
    FailedP errMsg -> putStrLn $ "[Parser error] " ++ errMsg
    OkP ast ->
      case exec ast of
        Left errMsg -> putStrLn $ "[Exec error] " ++ errMsg
        Right v -> putStrLn $ show v

{-|This is the main function of the interpreter-}
main :: IO()
main = runInputT defaultSettings loop
	where
   loop = do
    minput <- getInputLine "NjuScript> "
    case minput of
      Nothing -> outputStrLn "Bye-bye!"
      Just input -> (liftIO $ process input) >> loop