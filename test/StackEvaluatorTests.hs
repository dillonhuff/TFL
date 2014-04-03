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
	,evalExpr_MinusNum
	,evalExpr_PlusNum]

evalExpr_Num = testStackEval "3" (dummyNumExpr 3)

evalExpr_Bool = testStackEval "True" (dummyBoolExpr True)

evalExpr_Abs = testStackEval "\\x. x" (dummyAbsExpr "x" (dummyIExpr "x"))

evalExpr_MinusNum = testStackEval "- 3" (dummyNumExpr (-3))

evalExpr_PlusNum = testStackEval "+ 2 3" (dummyNumExpr 5)

testStackEval input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(evalExpr $ extractValue $ parseExpr input))