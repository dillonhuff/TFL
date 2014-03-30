module LexerTests(
	allLexerTests) where

import ErrorHandling
import Lexer
import Test.HUnit

allLexerTests = runTestTT tests

tests = TestList
	[lexer_Number]

lexer_Number =
	tokenTest "1239" [(Num 1239)]

tokenTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(map tok $ extractValue $ lexer input))