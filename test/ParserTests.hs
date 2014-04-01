module ParserTests(
	allParserTests) where

import ErrorHandling
import Parser
import Test.HUnit
import TypeSystem

allParserTests = runTestTT tests

tests = TestList
	[parseExpr_IExpr
	,parseExpr_OpExpr
	,parseExpr_NumExpr
	,parseExpr_BoolExprTrue
	,parseExpr_BoolExprFalse
	,parseExpr_parenExpr
	,parseExpr_absExpr
	,parseExpr_apExpr
	,parseExpr_multiApExpr
	,parseExpr_ifExpr
	,parseExpr_letExpr
	,typeOfExpr_Num
	,typeOfExpr_True
	,typeOfExpr_False
	,typeOfExpr_Abs
	,typeOfExpr_NumAp]

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

parseExpr_absExpr =
	parseExprTest "\\ x . (x 12)"
		(dummyAbsExpr "x" $
			ap (dummyIExpr "x") (dummyNumExpr 12))

parseExpr_apExpr =
	parseExprTest "(12 True)"
		(ap (dummyNumExpr 12) (dummyBoolExpr True))

parseExpr_multiApExpr =
	parseExprTest "x 12 (13 False)"
		(ap
			(ap (dummyIExpr "x") (dummyNumExpr 12))
			(ap (dummyNumExpr 13) (dummyBoolExpr False)))

parseExpr_parenExpr =
	parseExprTest "(n12)" (dummyIExpr "n12")

parseExpr_ifExpr =
	parseExprTest "if True then 12 else (- 1)"
		(ifExpr (dummyBoolExpr True)
			(dummyNumExpr 12)
			(ap (dummyOpExpr "-") (dummyNumExpr 1)))

parseExpr_letExpr =
	parseExprTest "let x = 34 in (\\y. (+ x 2) y)"
		(letExpr (dummyIExpr "x") (dummyNumExpr 34)
			(dummyAbsExpr "y"
				(ap (ap (ap (dummyOpExpr "+") (dummyIExpr "x")) (dummyNumExpr 2)) (dummyIExpr "y"))))

parseExprTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ parseExpr input))

typeOfExpr_Num = exprTypeTest "5483" INT

typeOfExpr_True = exprTypeTest "True" BOOL

typeOfExpr_False = exprTypeTest "False" BOOL

typeOfExpr_Abs = exprTypeTest "\\x . x" (Func (TV "vt0") (TV "vt0"))

typeOfExpr_NumAp = exprTypeTest "(\\y . y) 3" INT

exprTypeTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ typeOfExpr input))