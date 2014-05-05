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
  fun   { TokenFun }
  int   { TokenInt $$ }
  str   { TokenStr $$ }
  id    { TokenId $$ }
  '='   { TokenEq }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenMul }
  '/'   { TokenDiv }
  '('   { TokenLP }
  ')'   { TokenRP }

%monad { ParseAction }
%lexer { lexer } { TokenEOF }

%right in
%left '+' '-'
%left '*' '/'

%%

Exp :
      let id '=' Exp in Exp     { FunApp (Lambda $2 $6) $4 }
    | lam id Exp                { Lambda $2 $3 }
    | Exp '+' Exp               { Plus $1 $3 }
    | Exp '-' Exp               { Minus $1 $3 }
    | Exp '*' Exp               { Mul $1 $3 }
    | Exp '/' Exp               { Div $1 $3 }
    | '(' Exp ')'               { $2 }
    | Exp Exp                   { FunApp $1 $2 }
    | int                       { IntLit $1 }
    | str                       { StrLit $1 }
    | id                        { Var $1 }

{

parseError :: Token -> ParseAction a
parseError t = getLineNo >>= \l -> fail ("Line " ++ show l ++ ": Parse error after token " ++ show t)

}
