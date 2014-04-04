module Main(main) where

import LexerTests
import ParserTests
import StackEvaluatorTests

main = do
	allLexerTests
	allParserTests
	allStackEvaluatorTests