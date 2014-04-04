module Main(main) where

import Parser
import StackEvaluator
import System.IO

main = do
	handle <- openFile "test.tfl" ReadMode
	progText <- hGetContents handle
	putStrLn $ show $ typeOfProgram progText