{-|
 This module contains the lexer function
 -}
module NScriptParser.Lexer where

import NScriptParser.ParserMonad
import NScriptParser.AST
import Data.Char (isSpace, isAlpha, isDigit)

{-|The Token datatype -}
data Token
      = TokenLet
      | TokenIn
      | TokenLambda
      | TokenFun
      | TokenInt Int
      | TokenStr String
      | TokenId String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenMul
      | TokenDiv
      | TokenLP
      | TokenRP
      | TokenEOF
      deriving Eq

instance Show Token where
  show TokenLet = "let"
  show TokenIn = "in"
  show TokenLambda = "lambda"
  show TokenFun = "fun"
  show (TokenInt i) = show i
  show (TokenStr str) = "\"" ++ str ++ "\""
  show (TokenId str) = str
  show TokenEq = "="
  show TokenPlus = "+"
  show TokenMinus = "-"
  show TokenMul = "*"
  show TokenDiv = "/"
  show TokenLP = "("
  show TokenRP = ")"
  show TokenEOF = "EOF"

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
      | c == '"' = lexStr cont cs
    lexer1 (c:cs) = lexError ("unrecognized symbol " ++ [c]) cs

lexNum cont s = returnToken cont (TokenInt (read num)) rest
  where (num, rest) = span isDigit s

lexId cont s =
  case span isAlpha s of
    ("let", rest) -> returnToken cont TokenLet rest
    ("in", rest)  -> returnToken cont TokenIn rest
    ("lambda", rest) -> returnToken cont TokenLambda rest
    ("fun", rest) -> returnToken cont TokenFun rest
    (var, rest)   -> returnToken cont (TokenId var) rest

lexStr cont s = returnToken cont (TokenStr var) (tail rest)
  where
    (var, rest) = span (/= '\"') s


lexError errMsg = runP (getLineNo >>= \l -> fail ("Line " ++ show l ++ ": " ++ errMsg))