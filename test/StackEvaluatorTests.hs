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
	,evalExpr_AbsNum]

evalExpr_Num = testStackEval "3" (dummyNumExpr 3)

evalExpr_Bool = testStackEval "True" (dummyBoolExpr True)

evalExpr_Abs = testStackEval "\\x. x" (dummyAbsExpr "x" (dummyIExpr "x"))

evalExpr_AbsNum = testStackEval "(\\x . x) 3" (dummyNumExpr 3)

testStackEval input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(evalExpr $ extractValue $ parseExpr input))