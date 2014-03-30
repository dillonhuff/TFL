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
	,lexer_lparen
	,lexer_rparen
	,lexer_lambda
	,lexer_dot
	,lexer_overlapResAndId
	,lexer_plus
	,lexer_minus
	,lexer_times
	,lexer_divide
	,lexer_eqeq
	,lexer_lte
	,lexer_gte
	,lexer_lt
	,lexer_gt
	,lexer_not
	,lexer_and
	,lexer_or]

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

lexer_lparen =
	tokenTest "(" [LPAREN]

lexer_rparen =
	tokenTest ")" [RPAREN]

lexer_lambda =
	tokenTest "\\" [LAMBDA]

lexer_dot =
	tokenTest "." [DOT]

lexer_overlapResAndId =
	tokenTest "letter" [(I "letter")]

lexer_plus =
	tokenTest "+" [(Op "+")]

lexer_minus =
	tokenTest "-" [(Op "-")]

lexer_times =
	tokenTest "*" [(Op "*")]

lexer_divide =
	tokenTest "/" [(Op "/")]

lexer_eqeq =
	tokenTest "==" [(Op "==")]

lexer_lte =
	tokenTest "<=" [(Op "<=")]

lexer_gte =
	tokenTest ">=" [(Op ">=")]

lexer_lt =
	tokenTest "<" [(Op "<")]

lexer_gt =
	tokenTest ">" [(Op ">")]

lexer_not =
	tokenTest "~" [(Op "~")]

lexer_and =
	tokenTest "&&" [(Op "&&")]

lexer_or =
	tokenTest "||" [(Op "||")]

tokenTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(map tok $ extractValue $ lexer input))