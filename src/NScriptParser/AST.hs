{-|
 This module contains the datatype definitions of the Abstract Syntax Tree
 -}
module NScriptParser.AST where

data Program
      = DefStmt String Exp
      | ExpStmt Exp

data Exp
      = Let String Exp Exp
      | Lambda String Exp
      | FunApp Exp Exp
      | If Exp Exp Exp
      | Arith ArithOp Exp Exp
      | Compare CompareOp Exp Exp
      | IntLit Integer
      | BoolLit Bool
      | StrLit String
      | Var String
      deriving (Show, Eq)

data ArithOp
      = Plus
      | Minus
      | Mul
      | Div
      deriving (Show, Eq)

data CompareOp
      = Less
      | LessEq
      | Greater
      | GreaterEq
      | Equal
      | NotEqual
      deriving (Show, Eq)










