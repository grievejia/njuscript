{-|
 This module contains the datatype definitions of the Abstract Syntax Tree
 -}
module NScriptParser.AST where

data Exp
      = Let String Exp Exp
      | Lambda String Exp
      | FunApp Exp Exp
      | Plus Exp Exp
      | Minus Exp Exp
      | Mul Exp Exp
      | Div Exp Exp
      | IntLit Int
      | StrLit String
      | Var String
      deriving (Show, Eq)



