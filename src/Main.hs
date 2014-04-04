module Main(main) where

import ErrorHandling
import Parser
import StackEvaluator
import System.IO

main = do
	handle <- openFile "test.tfl" ReadMode
	progText <- hGetContents handle
	case typeOfProgram progText of
		Left err -> putStrLn $ show $ err
		Right t -> putStrLn $ show $ evalProgram $ extractValue $ parseExprDefs progText