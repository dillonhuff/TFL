module ParserTests(
	allParserTests) where

import ErrorHandling
import Parser
import Test.HUnit

allParserTests = runTestTT tests

tests = TestList
	[parseExpr_IExpr
	,parseExpr_OpExpr
	,parseExpr_NumExpr
	,parseExpr_BoolExprTrue
	,parseExpr_BoolExprFalse
	,parseExpr_parenExpr
	,parseExpr_lambdaExpr
	,parseExpr_absExpr]

parseExpr_IExpr =
	parseExprTest "n12" (dummyIExpr "n12")

parseExpr_OpExpr =
	parseExprTest "+" (dummyOpExpr "+")

parseExpr_NumExpr =
	parseExprTest "58263" (dummyNumExpr 58263)

parseExpr_BoolExprTrue =
	parseExprTest "True" (dummyBoolExpr True)

parseExpr_BoolExprFalse =
	parseExprTest "False" (dummyBoolExpr False)

parseExpr_lambdaExpr =
	parseExprTest "\\ x . (x 12)"
		(dummyAbsExpr "x" $
			ap (dummyIExpr "x") (dummyNumExpr 12))

parseExpr_absExpr =
	parseExprTest "(12 True)"
		(ap (dummyNumExpr 12) (dummyBoolExpr True))

parseExpr_parenExpr =
	parseExprTest "(n12)" (dummyIExpr "n12")

parseExprTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ parseExpr input))