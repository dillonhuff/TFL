module Main(main) where

import LexerTests
import ParserTests
import StackEvaluatorTests
import TypeSystemTests

main = do
	allLexerTests
	allParserTests
	allTypeSystemTests
	allStackEvaluatorTests