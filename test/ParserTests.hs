module ParserTests(
	allParserTests) where

import ErrorHandling
import Parser
import Test.HUnit

allParserTests = runTestTT tests

tests = TestList
	[parseExpr_IExpr
	,parseExpr_OpExpr
	,parseExpr_NumExpr]

parseExpr_IExpr =
	parseExprTest "n12" (dummyIExpr "n12")

parseExpr_OpExpr =
	parseExprTest "+" (dummyOpExpr "+")

parseExpr_NumExpr =
	parseExprTest "58263" (dummyNumExpr 58263)

parseExprTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ parseExpr input))