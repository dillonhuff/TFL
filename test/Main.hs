module Main(main) where

import LexerTests
import ParserTests
import TypeSystemTests

main = do
	allLexerTests
	allParserTests
	allTypeSystemTests