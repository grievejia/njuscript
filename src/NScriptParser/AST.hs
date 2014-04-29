{-|
 This module contains the datatype definitions of the Abstract Syntax Tree
 -}
module NScriptParser.AST where

data Exp
      = Let String Exp Exp
      | Plus Exp Exp
      | Minus Exp Exp
      | Mul Exp Exp
      | Div Exp Exp
      | Int Int
      | Var String
      deriving (Show, Eq)

data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenMul
      | TokenDiv
      | TokenLP
      | TokenRP
      | TokenEOF
      deriving (Show, Eq)

