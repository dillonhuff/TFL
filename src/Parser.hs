module Parser(
	parseExpr,
	dummyIExpr, dummyOpExpr, dummyNumExpr) where

import ErrorHandling
import Lexer
import Text.Parsec

data Expr
	= IExpr PosTok
	| OpExpr PosTok
	| NumExpr PosTok
	deriving (Eq, Show)

dummyIExpr :: String -> Expr
dummyIExpr name = IExpr (dummyPosTok (I name))

dummyOpExpr :: String -> Expr
dummyOpExpr name = OpExpr (dummyPosTok (Op name))

dummyNumExpr :: Int -> Expr
dummyNumExpr val = NumExpr (dummyPosTok (Num val))

parseExpr :: String -> ThrowsError Expr
parseExpr programText = case lexer programText of
	Left err -> Left err
	Right toks -> case parse pExpr "Parser" toks of
		Left err -> Left $ Parse err
		Right expr -> Right expr

pExpr = do
	expr <- pIExpr
		<|> pOpExpr
		<|> pNumExpr
	return expr

pIExpr = do
	ident <- idTok
	return $ IExpr ident

pOpExpr = do
	oper <- opTok
	return $ OpExpr oper

pNumExpr = do
	num <- numTok
	return $ NumExpr num

opTok :: (Monad m) => ParsecT [PosTok] u m PosTok
opTok = tokenPrim show updatePos opTok
	where
		opTok pt = if isOpTok (tok pt) then Just pt else Nothing

idTok :: (Monad m) => ParsecT [PosTok] u m PosTok
idTok = tokenPrim show updatePos idTok
	where
		idTok pt = if isIdTok (tok pt) then Just pt else Nothing

numTok :: (Monad m) => ParsecT [PosTok] u m PosTok
numTok = tokenPrim show updatePos numTok
	where
		numTok pt = if isNumTok (tok pt) then Just pt else Nothing

boolTok :: (Monad m) => ParsecT [PosTok] u m PosTok
boolTok = tokenPrim show updatePos boolTok
	where
		boolTok pt = if isBoolTok (tok pt) then Just pt else Nothing

lTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
lTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos