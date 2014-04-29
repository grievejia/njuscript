{
{-|
 This module contains the parser.
 We use a parser generator approach rather than parser combinator for efficiency parsing.
 The generator we used here is happy
 -}
module NScriptParser.Parser (parser) where
import NScriptParser.AST
import NScriptParser.Lexer


}

%name parser
%tokentype { Token }
%error { parseError }

%token 
  let   { TokenLet }
  in    { TokenIn }
  int   { TokenInt $$ }
  var   { TokenVar $$ }
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
      let var '=' Exp in Exp    { Let $2 $4 $6 }
    | Exp '+' Exp               { Plus $1 $3 }
    | Exp '-' Exp               { Minus $1 $3 }
    | Exp '*' Exp               { Mul $1 $3 }
    | Exp '/' Exp               { Div $1 $3 }
    | '(' Exp ')'               { $2 }
    | int                       { Int $1 }
    | var                       { Var $1 }

{

parseError :: Token -> ParseAction a
parseError t = getLineNo >>= \l -> fail ("Line " ++ show l ++ ": Parse error after token " ++ show t)

}
