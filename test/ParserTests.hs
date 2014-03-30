module ParserTests(
	allParserTests) where

import Test.HUnit

allParserTests = runTestTT tests

tests = TestList
	[]