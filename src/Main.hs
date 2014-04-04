module Main(main) where

import Parser
import StackEvaluator
import System.IO

main = do
	handle <- openFile "test.tfl" ReadMode
	progText <- hGetContents handle
	case parseExprDefs progText of
		Left err -> putStrLn $ show err
		Right defs -> putStrLn $ show $ typeOfProg defs