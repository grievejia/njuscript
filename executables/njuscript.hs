import NScriptParser.Lexer
import NScriptParser.Parser
import NScriptParser.AST

import System.Environment

{-|This is the main function of the program-}
main :: IO()
main =
  do
    text <- getContents
    let rs = runP parser text 0
    case rs of
      OkP ast -> print ast
      FailedP errMsg -> putStrLn errMsg
