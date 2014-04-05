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
	,typeOfExpr_NumAp
	,typeOfExpr_IfExpr
	,typeOfExpr_ComplexIf
	,typeOfExpr_LetExpr
	,typeOfExpr_PlusExpr
	,typeOfExpr_TimesExpr
	,typeOfExpr_DivideExpr
	,typeOfExpr_MinusExpr
	,typeOfExpr_EqExpr
	,typeOfExpr_LTEExpr
	,typeOfExpr_GTEExpr
	,typeOfExpr_LTExpr
	,typeOfExpr_GTExpr
	,typeOfExpr_NotExpr
	,typeOfExpr_AndExpr
	,typeOfExpr_OrExpr
	,typeOfExpr_IfComp
	,typeOfExpr_ArithAndBool
	,typeOfExpr_Nil
	,typeOfExpr_SimpleList
	,typeOfExpr_ManyCons
	,typeOfExpr_Head
	,typeOfExpr_Tail
	,typeOfExpr_NestedList
	,parseExprDefs_NumDef
	,parseExprDefs_Func]

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

typeOfExpr_IfExpr = exprTypeTest "if True then 12 else 98" INT

typeOfExpr_ComplexIf = exprTypeTest "if True then (\\x . x) 4 else 123" INT

typeOfExpr_LetExpr = exprTypeTest "let x = True in x" BOOL

typeOfExpr_PlusExpr = exprTypeTest "+" (Func INT (Func INT INT))

typeOfExpr_TimesExpr = exprTypeTest "*" (Func INT (Func INT INT))

typeOfExpr_DivideExpr = exprTypeTest "/" (Func INT (Func INT INT))

typeOfExpr_MinusExpr = exprTypeTest "-" (Func INT INT)

typeOfExpr_EqExpr = exprTypeTest "==" (Func INT (Func INT BOOL))

typeOfExpr_LTEExpr = exprTypeTest "<=" (Func INT (Func INT BOOL))

typeOfExpr_GTEExpr = exprTypeTest ">=" (Func INT (Func INT BOOL))

typeOfExpr_LTExpr = exprTypeTest "<" (Func INT (Func INT BOOL))

typeOfExpr_GTExpr = exprTypeTest ">" (Func INT (Func INT BOOL))

typeOfExpr_NotExpr = exprTypeTest "~" (Func BOOL BOOL)

typeOfExpr_AndExpr = exprTypeTest "&&" (Func BOOL (Func BOOL BOOL))

typeOfExpr_OrExpr = exprTypeTest "||" (Func BOOL (Func BOOL BOOL))

typeOfExpr_Nil = exprTypeTest "nil" (TV "ct0")

typeOfExpr_SimpleList = exprTypeTest "cons True nil" (List BOOL)

typeOfExpr_ManyCons = exprTypeTest
	"cons 3 (cons 4 (cons (-2) (cons (+ 1 (-1)) nil)))"
	(List INT)

typeOfExpr_Head = exprTypeTest "head (cons 3 nil)" INT

typeOfExpr_Tail = exprTypeTest "tail (cons (* 3 2) (cons 5 nil))" (List INT)

typeOfExpr_NestedList = exprTypeTest
	"head (cons (cons 2 nil) (cons (cons 3 (cons 4 (cons 5 nil))) nil))"
	(List INT)

typeOfExpr_IfComp =
	exprTypeTest "if <= 12 2 then && True (|| False True) else True" BOOL

typeOfExpr_ArithAndBool =
	exprTypeTest "<= (+ (* 2 3) (- 2)) (- (/ 2 (+ 2 3)))" BOOL

exprTypeTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ typeOfExpr input))

parseExprDefs_NumDef =
	exprDefTest "k = 12;" [exprDef [dummyIExpr "k"] (dummyNumExpr 12)]

parseExprDefs_Func =
	exprDefTest "square x = * x x;"
		[exprDef [dummyIExpr "square"] (dummyAbsExpr "x"
			(ap (ap (dummyOpExpr "*") (dummyIExpr "x")) (dummyIExpr "x")))]

exprDefTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ parseExprDefs input))