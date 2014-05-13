import NScriptParser.ParserMonad
import NScriptParser.Parser
import NScriptParser.AST

import NScriptInterpreter.Interpreter

import System.Environment

{-|This is the main function of the interpreter-}
main :: IO()
main =
  do
    text <- getContents
    let rs = runP parser text 0
    case rs of
      FailedP errMsg -> putStrLn errMsg
      OkP (ExpStmt ast) -> print $ exec ast
      
