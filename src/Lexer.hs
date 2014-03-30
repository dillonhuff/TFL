module Lexer(
	PosTok(PT), tok, pos, dummyPosTok,
	lexer,
	Tok(I, Num, Op, Boolean, LET, EQUAL, IN, IF, THEN, ELSE, LPAREN, RPAREN, LAMBDA, DOT),
	isOpTok, isNumTok, isIdTok, isBoolTok) where

import ErrorHandling
import Text.Parsec.Pos
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

dummyPosTok :: Tok -> PosTok
dummyPosTok t = PT t (newPos "DUMMY" 0 0)

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
	| LPAREN
	| RPAREN
	| LAMBDA
	| DOT
	deriving (Eq, Show)

isOpTok (Op _) = True
isOpTok _ = False

isIdTok (I _) = True
isIdTok _ = False

isNumTok (Num _) = True
isNumTok _ = False

isBoolTok (Boolean _) = True
isBoolTok _ = False

resToTok =
	[("let", LET), ("=", EQUAL), ("in", IN), ("if", IF)
	,("then", THEN), ("else", ELSE), ("(", LPAREN)
	,(")", RPAREN), ("\\", LAMBDA), (".", DOT)]

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
		<|> try pOp
		<|> pIdentOrRes
		<|> pBool
	return t

pNum = do
	pos <- getPosition
	digs <- many1 digit
	return $ PT (Num $ read digs) pos

pOp = do
	pos <- getPosition
	op <- string "+"
		<|> string "-"
		<|> string "*"
		<|> string "/"
		<|> string "=="
		<|> try (string "<=")
		<|> try (string ">=")
		<|> string "<"
		<|> string ">"
		<|> string "~"
		<|> string "&&"
		<|> string "||"
	return $ PT (Op op) pos

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
		<|> string "\\"
		<|> string "."
		<|> string "("
		<|> string ")"
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
