{-|
 This module contains the lexer function
 -}
module NScriptParser.Lexer where

import NScriptParser.AST
import Data.Char (isSpace, isAlpha, isDigit)

{-|
 Define exception monad to do error handling as well as line numbers
-}
data ParseResult a = OkP a | FailedP String
type LineNumber = Int
newtype ParseAction a = ParseAction (String -> LineNumber -> ParseResult a)

runP :: ParseAction a -> String -> LineNumber -> ParseResult a
runP (ParseAction f) = f

instance Monad ParseAction where
  return m = ParseAction $ \_ _ -> OkP m
  m >>= k = ParseAction $ \s l -> case runP m s l of
    OkP a -> runP (k a) s l
    FailedP err -> FailedP err
  fail s = ParseAction $ \_ _ -> FailedP s

getLineNo :: ParseAction LineNumber
getLineNo = ParseAction $ \_ l -> OkP l

returnToken :: (t -> ParseAction a) -> t -> String -> LineNumber -> ParseResult a
returnToken cont tok = runP (cont tok)

{-|
 Monadic lexer
 -}
lexer :: (Token -> ParseAction a) -> ParseAction a
lexer cont = ParseAction lexer1 where
    lexer1 "" = returnToken cont TokenEOF ""
    lexer1 ('\n':cs) = \line -> returnToken lexer cont cs (line+1)
    lexer1 ('=':cs) = returnToken cont TokenEq cs
    lexer1 ('+':cs) = returnToken cont TokenPlus cs
    lexer1 ('-':cs) = returnToken cont TokenMinus cs
    lexer1 ('*':cs) = returnToken cont TokenMul cs
    lexer1 ('/':cs) = returnToken cont TokenDiv cs
    lexer1 ('(':cs) = returnToken cont TokenLP cs
    lexer1 (')':cs) = returnToken cont TokenRP cs
    lexer1 input@(c:cs)
      | isSpace c = returnToken lexer cont cs
      | isAlpha c = lexId cont input
      | isDigit c = lexNum cont input
    lexer1 (c:cs) = lexError ("unrecognized symbol " ++ [c]) cs

lexNum cont s = returnToken cont (TokenInt (read num)) rest
  where (num, rest) = span isDigit s

lexId cont s =
  case span isAlpha s of
    ("let", rest) -> returnToken cont TokenLet rest
    ("in", rest)  -> returnToken cont TokenIn rest
    (var, rest)   -> returnToken cont (TokenVar var) rest

lexError errMsg = runP (getLineNo >>= \l -> fail ("Line " ++ show l ++ ": " ++ errMsg))