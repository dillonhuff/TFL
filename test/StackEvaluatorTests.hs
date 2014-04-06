module StackEvaluatorTests(
	allStackEvaluatorTests) where

import ErrorHandling
import Parser
import StackEvaluator
import Test.HUnit

allStackEvaluatorTests = runTestTT tests

tests = TestList
	[evalExpr_Num
	,evalExpr_Bool
	,evalExpr_Abs
	,evalExpr_AbsSub
	,evalExpr_BoolAbs
	,evalExpr_ArithAbs
	,evalExpr_ManyNestedAbs
	,evalExpr_HighOrdFunc
	,evalExpr_MinusNum
	,evalExpr_PlusNum
	,evalExpr_TimesNum
	,evalExpr_DivNum
	,evalExpr_ArithExpr
	,evalExpr_NotExpr
	,evalExpr_AndExpr
	,evalExpr_OrExpr
	,evalExpr_EQ
	,evalExpr_LTE
	,evalExpr_GTE
	,evalExpr_LT
	,evalExpr_GT
	,evalExpr_Complicated
	,evalExpr_SimpleIf
	,evalExpr_ComplicatedIf
	,evalExpr_SimpleLet
	,evalExpr_LetBoolOp
	,evalExpr_LetIf
	,evalExpr_LetAbs
	,evalExpr_LetAbsNotSameVar
	,evalExpr_ConsExpr
	,evalExpr_TailExpr
	,evalExpr_HeadExpr
	,evalExpr_IsNil]

evalExpr_Num = testStackEval "3" (dummyNumExpr 3)

evalExpr_Bool = testStackEval "True" (dummyBoolExpr True)

evalExpr_Abs = testStackEval "\\x. x" (dummyAbsExpr "x" (dummyIExpr "x"))

evalExpr_AbsSub = testStackEval "(\\k . k) 2" (dummyNumExpr 2)

evalExpr_BoolAbs = testStackEval "(\\x. (|| x x)) True" (dummyBoolExpr True)

evalExpr_ArithAbs = testStackEval "(\\x . (\\y . (+ x y))) 2 3" (dummyNumExpr 5)

evalExpr_HighOrdFunc = testStackEval "(\\f . (\\x . (f x))) (* 2) 3" (dummyNumExpr 6)

evalExpr_ManyNestedAbs =
	testStackEval
		"(\\x. (\\y . (* (- ((\\ u . u) 3)) (+ x y)))) 89 (- 23)"
		(dummyNumExpr (-198))

evalExpr_MinusNum = testStackEval "- 3" (dummyNumExpr (-3))

evalExpr_PlusNum = testStackEval "+ 2 3" (dummyNumExpr 5)

evalExpr_TimesNum = testStackEval "* 12 89" (dummyNumExpr (1068))

evalExpr_DivNum = testStackEval "/ 17 3" (dummyNumExpr 5)

evalExpr_ArithExpr =
	testStackEval "* 4 (+ (- 3) 7))" (dummyNumExpr 16)

evalExpr_NotExpr =
	testStackEval "~True" (dummyBoolExpr False)

evalExpr_AndExpr =
	testStackEval "&& True False" (dummyBoolExpr False)

evalExpr_OrExpr =
	testStackEval "|| False True" (dummyBoolExpr True)

evalExpr_EQ =
	testStackEval "== 34 12" (dummyBoolExpr False)

evalExpr_GTE =
	testStackEval ">= (-78) (-123)" (dummyBoolExpr True)

evalExpr_LTE =
	testStackEval "<= 9 14" (dummyBoolExpr True)

evalExpr_GT =
	testStackEval "> 4 12" (dummyBoolExpr False)

evalExpr_LT =
	testStackEval "< 34908 176" (dummyBoolExpr False)

evalExpr_Complicated =
	testStackEval
		"|| (== (+ (- (- 1)) (* 2 12)) (- 12)) (~ (<= (+ 2 3) (* 3 (-2))))"
		(dummyBoolExpr True)

evalExpr_SimpleIf =
	testStackEval "if True then 9 else 2"
	(dummyNumExpr 9)

evalExpr_ComplicatedIf =
	testStackEval
		"if ~(~(|| (>= 2 (-(* 2 (-9)))) False)) then 2 else + (-(+ (-2) (/ 6 (*2 3)))) (* 2 3)"
		(dummyNumExpr 7)

evalExpr_SimpleLet =
	testStackEval "let k = 3 in k" (dummyNumExpr 3)

evalExpr_LetBoolOp =
	testStackEval "let x = True in (&& True x)" (dummyBoolExpr True)

evalExpr_LetIf =
	testStackEval
		"let u = (- 12) in if (<= 3 u) then (+ 2 5) else (+ (-9) u)"
		(dummyNumExpr (-21))

evalExpr_LetAbs =
	testStackEval "let t = 3 in (\\t . (* t t))" (dummyNumExpr 9)

evalExpr_LetAbsNotSameVar =
	testStackEval "let y = 3 in (\\t . y)" (dummyAbsExpr "t" (dummyNumExpr 3))

evalExpr_ConsExpr =
	testStackEval "cons 2 nil" (listExpr (dummyNumExpr 2) nilExpr)

evalExpr_TailExpr =
	testStackEval "tail (cons True (cons False nil))" (listExpr (dummyBoolExpr False) nilExpr)

evalExpr_IsNil =
	testStackEval "isNil nil" (dummyBoolExpr True)

evalExpr_HeadExpr =
	testStackEval "head (cons (cons 2 nil) (cons (cons 3 (cons 4 nil)) nil))"
		(listExpr (dummyNumExpr 2) nilExpr)

testStackEval input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(evalExpr $ extractValue $ parseExpr input))