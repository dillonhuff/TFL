module Lexer(
	tok, pos,
	lexer,
	Tok(I, Num, Op, Boolean, LET, EQUAL, IN, IF, THEN, ELSE)) where

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
	| LET
	| EQUAL
	| IN
	| IF
	| THEN
	| ELSE
	deriving (Eq, Show)

resToTok =
	[("let", LET), ("=", EQUAL), ("in", IN), ("if", IF)
	,("then", THEN), ("else", ELSE)]

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
		<|> pIdentOrRes
		<|> pBool
	return t

pNum = do
	pos <- getPosition
	digs <- many1 digit
	return $ PT (Num $ read digs) pos

pIdentOrRes = do
	pos <- getPosition
	idOrRes <- pId
		<|> pResName
	return $ case lookup idOrRes resToTok of
		Just t -> PT t pos
		Nothing -> PT (I idOrRes) pos

pId = do
	startChar <- lower
	rest <- many alphaNum
	return (startChar:rest)

pResName = do
	rName <- string "="
	return rName

pBool = do
	b <- pTrue
		<|> pFalse
	return b

pTrue = do
	pos <- getPosition
	val <- string "True"
	return $ PT (Boolean True) pos

pFalse = do
	pos <- getPosition
	val <- string "False"
	return $ PT (Boolean False) pos
