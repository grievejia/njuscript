{
{-|
 This module contains the parser.
 We use a parser generator approach rather than parser combinator for efficiency parsing.
 The generator we used here is happy
 -}
module NScriptParser.Parser (parser) where
import NScriptParser.AST
import NScriptParser.Lexer
import NScriptParser.ParserMonad

}

%name parser
%tokentype { Token }
%error { parseError }

%token 
  let   { TokenLet }
  in    { TokenIn }
  lam   { TokenLambda }
  def   { TokenDef }
  if    { TokenIf }
  then  { TokenThen }
  else  { TokenElse }
  int   { TokenInt $$ }
  str   { TokenStr $$ }
  id    { TokenId $$ }
  '='   { TokenAssign }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenMul }
  '/'   { TokenDiv }
  lt    { TokenLT }
  le    { TokenLE }
  gt    { TokenGT }
  ge    { TokenGE }
  eq    { TokenEQ }
  ne    { TokenNE }
  true  { TokenTrue }
  false { TokenFalse }
  '('   { TokenLP }
  ')'   { TokenRP }

%monad { ParseAction }
%lexer { lexer } { TokenEOF }

%right in
%nonassoc lam let in if then else
%nonassoc gt ge lt le eq ne
%left '+' '-'
%left '*' '/'

%%

Program :
      def id '=' Exp              { DefStmt $2 $4 }
    | def id LamDef               { DefStmt $2 $3}
    | Exp                         { ExpStmt $1 }

LamDef :
      id LamDef                   { Lambda $1 $2 }
    | id '=' Exp                  { Lambda $1 $3 }

Exp :
      let id '=' Exp in Exp       { FunApp (Lambda $2 $6) $4 }
    | lam id Exp                  { Lambda $2 $3 }
    | if Exp then Exp else Exp    { If $2 $4 $6 }
    | Exp2                        { $1 }

Exp2 :
      Exp2 lt Exp3                { Compare Less $1 $3 }
    | Exp2 le Exp3                { Compare LessEq $1 $3 }
    | Exp2 gt Exp3                { Compare Greater $1 $3 }
    | Exp2 ge Exp3                { Compare GreaterEq $1 $3 }
    | Exp2 eq Exp3                { Compare Equal $1 $3 }
    | Exp2 ne Exp3                { Compare NotEqual $1 $3 }
    | Exp3                        { $1 }

Exp3 :
      Exp3 '+' Exp4               { Arith Plus $1 $3 }
    | Exp3 '-' Exp4               { Arith Minus $1 $3 }
    | Exp3 '*' Exp4               { Arith Mul $1 $3 }
    | Exp3 '/' Exp4               { Arith Div $1 $3 }
    | Exp4                        { $1 }

Exp4 :
      Exp4 Exp5                   { FunApp $1 $2 }
    | Exp5                        { $1 }

Exp5 :
      int                         { IntLit $1 }
    | str                         { StrLit $1 }
    | id                          { Var $1 }
    | true                        { BoolLit True }
    | false                       { BoolLit False }
    | '(' Exp ')'                 { $2 }

{

parseError :: Token -> ParseAction a
parseError t = getLineNo >>= \l -> fail ("Line " ++ show l ++ ": Parse error after token " ++ show t)

}
