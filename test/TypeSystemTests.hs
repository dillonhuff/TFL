module TypeSystemTests(
	allTypeSystemTests) where

import ErrorHandling
import Parser
import Test.HUnit
import TypeSystem

allTypeSystemTests = runTestTT tests

tests = TestList
	[inferType_Num
	,inferType_BoolTrue
	,inferType_BoolFalse
	,inferTypeTest_BoolId
	,inferTypeTest_PlusOp
	,inferTypeTest_Abs]

inferType_Num =
	inferTypeTest [] "12" ([], T INT)

inferType_BoolTrue =
	inferTypeTest [] "True" ([], T BOOL)

inferType_BoolFalse =
	inferTypeTest [] "False" ([], T BOOL)

inferTypeTest_BoolId =
	inferTypeTest [(dummyIExpr "x", BOOL)] "x" ([], T BOOL)

inferTypeTest_PlusOp =
	inferTypeTest [(dummyOpExpr "+", Func (Func INT INT) INT)]
		"+" ([], T $ Func (Func INT INT) INT)

inferTypeTest_Abs =
	inferTypeTest [] "\\ x. x" ([], T $ Func (TypeVar "t0") (TypeVar "t0"))

inferTypeTest context input expected = TestCase
	(assertEqual ("Input: " ++ input)
		expected
		(extractValue $ inferType context input))