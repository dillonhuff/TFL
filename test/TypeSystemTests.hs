module TypeSystemTests(
	allTypeSystemTests) where

import Test.HUnit

allTypeSystemTests = runTestTT tests

tests = TestList
	[]