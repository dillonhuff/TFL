module Lexer(
	tok, pos,
	lexer,
	Tok(I, Num, Op, Boolean)) where

import ErrorHandling
import Text.ParserCombinators.Parsec

data PosTok = PT Tok SourcePos
	
instance Show PosTok where
	show = showPT

showPT :: PosTok -> String
showPT (PT t _) = show t

tok :: PosTok -> Tok
tok (PT t _) = t

pos :: PosTok -> SourcePos
pos (PT _ p) = p

instance Eq PosTok where
	(==) = ptEq

ptEq (PT t1 _) (PT t2 _) = t1 == t2

data Tok
	= I String
	| Op String
	| Num Int
	| Boolean Bool
	deriving (Eq, Show)

lexer :: String -> ThrowsError [PosTok]
lexer str = case parse pToks "TFL" str of
	Left err -> Left $ Parse err
	Right toks -> Right toks

pToks = do
	spaces
	ts <- endBy pTok spaces
	return ts

pTok = do
	t <- pNum
	return t

pNum = do
	pos <- getPosition
	digs <- many1 digit
	return $ PT (Num $ read digs) pos