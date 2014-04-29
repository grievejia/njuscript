module UnitTest where

import NScriptParser.AST
import NScriptParser.Lexer
import NScriptParser.Parser

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = testGroup "HUnit tests" [
  testCase "lexerTest space"       $ lexer "   " @?= [],
  testCase "lexerTest id"          $ lexer "a b" @?= [TokenVar "a", TokenVar "b"],
  testCase "lexerTest paren"       $ lexer "(010)" @?= [TokenLP, TokenInt 10, TokenRP]
 ]