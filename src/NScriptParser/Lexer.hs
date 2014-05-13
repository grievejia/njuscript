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
      | TokenDef
      | TokenIf
      | TokenThen
      | TokenElse
      | TokenInt Integer
      | TokenStr String
      | TokenId String
      | TokenAssign
      | TokenPlus
      | TokenMinus
      | TokenMul
      | TokenDiv
      | TokenLT
      | TokenLE
      | TokenGT
      | TokenGE
      | TokenEQ
      | TokenNE
      | TokenTrue
      | TokenFalse
      | TokenLP
      | TokenRP
      | TokenEOF
      deriving Eq

instance Show Token where
  show TokenLet = "let"
  show TokenIn = "in"
  show TokenLambda = "lambda"
  show TokenDef = "def"
  show TokenIf = "if"
  show TokenThen = "then"
  show TokenElse = "else"
  show (TokenInt i) = show i
  show (TokenStr str) = "\"" ++ str ++ "\""
  show (TokenId str) = str
  show TokenAssign = "="
  show TokenPlus = "+"
  show TokenMinus = "-"
  show TokenMul = "*"
  show TokenDiv = "/"
  show TokenLT = "<"
  show TokenLE = "<="
  show TokenGT = ">"
  show TokenGE = ">="
  show TokenEQ = "=="
  show TokenNE = "!="
  show TokenTrue = "true"
  show TokenFalse = "false"
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
    lexer1 ('+':cs) = returnToken cont TokenPlus cs
    lexer1 ('-':cs) = returnToken cont TokenMinus cs
    lexer1 ('*':cs) = returnToken cont TokenMul cs
    lexer1 ('/':cs) = returnToken cont TokenDiv cs
    lexer1 ('(':cs) = returnToken cont TokenLP cs
    lexer1 (')':cs) = returnToken cont TokenRP cs
    lexer1 ('<':'=':cs) = returnToken cont TokenLE cs
    lexer1 ('<':cs) = returnToken cont TokenLT cs
    lexer1 ('>':'=':cs) = returnToken cont TokenGE cs
    lexer1 ('>':cs) = returnToken cont TokenGT cs
    lexer1 ('=':'=':cs) = returnToken cont TokenEQ cs
    lexer1 ('!':'=':cs) = returnToken cont TokenNE cs
    lexer1 ('=':cs) = returnToken cont TokenAssign cs
    lexer1 input@(c:cs)
      | isSpace c = returnToken lexer cont cs
      | isAlpha c = lexId cont input
      | isDigit c = lexNum cont input
      | c == '"' = lexStr cont cs
    lexer1 (c:cs) = lexError ("unrecognized symbol " ++ [c]) cs

lexNum cont s = returnToken cont (TokenInt (read num)) rest
  where (num, rest) = span isDigit s

lexId cont s =
  case span (\x -> isAlpha x || isDigit x) s of
    ("let", rest) -> returnToken cont TokenLet rest
    ("in", rest)  -> returnToken cont TokenIn rest
    ("lambda", rest) -> returnToken cont TokenLambda rest
    ("def", rest) -> returnToken cont TokenDef rest
    ("if", rest) -> returnToken cont TokenIf rest
    ("then", rest) -> returnToken cont TokenThen rest
    ("else", rest) -> returnToken cont TokenElse rest
    ("true", rest) -> returnToken cont TokenTrue rest
    ("false", rest) -> returnToken cont TokenFalse rest
    (var, rest)   -> returnToken cont (TokenId var) rest

lexStr cont s = returnToken cont (TokenStr var) (tail rest)
  where
    (var, rest) = span (/= '\"') s


lexError errMsg = runP (getLineNo >>= \l -> fail ("Line " ++ show l ++ ": " ++ errMsg))