module Parser(
	parseExpr,
	dummyIExpr, dummyOpExpr, dummyNumExpr, dummyBoolExpr, dummyAbsExpr, ap) where

import ErrorHandling
import Lexer
import Text.Parsec

data Expr
	= IExpr PosTok
	| OpExpr PosTok
	| NumExpr PosTok
	| BoolExpr PosTok
	| AbsExpr Expr Expr
	| ApExpr Expr Expr
	deriving (Eq, Show)

dummyIExpr :: String -> Expr
dummyIExpr name = IExpr (dummyPosTok (I name))

dummyOpExpr :: String -> Expr
dummyOpExpr name = OpExpr (dummyPosTok (Op name))

dummyNumExpr :: Int -> Expr
dummyNumExpr val = NumExpr (dummyPosTok (Num val))

dummyBoolExpr :: Bool -> Expr
dummyBoolExpr val = BoolExpr (dummyPosTok (Boolean val))

dummyAbsExpr :: String -> Expr -> Expr
dummyAbsExpr name term = AbsExpr (dummyIExpr name) term

ap :: Expr -> Expr -> Expr
ap t1 t2 = ApExpr t1 t2

parseExpr :: String -> ThrowsError Expr
parseExpr programText = case lexer programText of
	Left err -> Left err
	Right toks -> case parse pExpr "Parser" toks of
		Left err -> Left $ Parse err
		Right expr -> Right expr

pExpr = do
	expr <- pParenSEExpr
		<|> pSEExpr
		<|> pSExpr
	return expr

pSExpr = do
	expr <- pParenExpr
		<|> pIExpr
		<|> pOpExpr
		<|> pNumExpr
		<|> pBoolExpr
		<|> pAbsExpr
	return expr

pParenSEExpr = do
	tflTok LPAREN
	psexpr <- pSEExpr
	tflTok RPAREN
	return $ psexpr

pSEExpr = do
	sExpr <- pSEExpr
	expr <- pExpr
	return $ ap sExpr expr

pParenExpr = do
	tflTok LPAREN
	expr <- pExpr
	tflTok RPAREN
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

pBoolExpr = do
	val <- boolTok
	return $ BoolExpr val

pAbsExpr = do
	tflTok LAMBDA
	v <- pIExpr
	tflTok DOT
	expr <- pExpr
	return $ AbsExpr v expr

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

tflTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
tflTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos