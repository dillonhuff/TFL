module LexerTests(
	allLexerTests) where

import ErrorHandling
import Lexer
import Test.HUnit

allLexerTests = runTestTT tests

tests = TestList
	[lexer_Number
	,lexer_Id
	,lexer_True
	,lexer_False
	,lexer_let
	,lexer_equal
	,lexer_in
	,lexer_if
	,lexer_then
	,lexer_else
	,lexer_overlapResAndId]

lexer_Number =
	tokenTest "1239" [(Num 1239)]

lexer_Id =
	tokenTest "a" [(I "a")]

lexer_True =
	tokenTest "True" [(Boolean True)]

lexer_False =
	tokenTest "False" [(Boolean False)]

lexer_let =
	tokenTest "let" [LET]

lexer_equal =
	tokenTest "=" [EQUAL]

lexer_in =
	tokenTest "in" [IN]

lexer_if =
	tokenTest "if" [IF]

lexer_then =
	tokenTest "then" [THEN]

lexer_else =
	tokenTest "else" [ELSE]

lexer_overlapResAndId =
	tokenTest "letter" [(I "letter")]

tokenTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(map tok $ extractValue $ lexer input))