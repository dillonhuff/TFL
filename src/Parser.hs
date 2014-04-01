module Parser(
	position,
	parseExpr,
	typeOfExpr,
	dummyIExpr, dummyOpExpr, dummyNumExpr,
	dummyBoolExpr, dummyAbsExpr, ifExpr, letExpr, ap) where

import ErrorHandling
import Lexer
import Text.Parsec
import TypeSystem

data Expr
	= IExpr PosTok
	| OpExpr PosTok
	| NumExpr PosTok
	| BoolExpr PosTok
	| AbsExpr Expr Expr
	| ApExpr Expr Expr
	| IfExpr Expr Expr Expr
	| LetExpr Expr Expr Expr
	deriving (Eq, Show)

position :: Expr -> SourcePos
position (IExpr tok) = pos tok
position (OpExpr tok) = pos tok
position (NumExpr tok) = pos tok
position (BoolExpr tok) = pos tok
position (AbsExpr ident e) = position ident
position (ApExpr t1 t2) = position t1

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

ifExpr :: Expr -> Expr -> Expr -> Expr
ifExpr cond e1 e2 = IfExpr cond e1 e2

letExpr :: Expr -> Expr -> Expr -> Expr
letExpr ident sub e = LetExpr ident sub e

ap :: Expr -> Expr -> Expr
ap t1 t2 = ApExpr t1 t2

typeOfExpr :: String -> ThrowsError Type
typeOfExpr exprStr = case parseExpr exprStr of
	Left err -> Left err
	Right parsedExpr -> Right $ typeOf parsedExpr

typeOf :: Expr -> Type
typeOf expr = doSub sub (TV "t0")
	where
		constraints = typeConstraints expr
		sub = unify constraints

typeConstraints :: Expr -> [(Type, Type)]
typeConstraints expr = tc expr "t0" []

tc :: Expr -> String -> [(Expr, Type)] -> [(Type, Type)]
tc e@(NumExpr _) typeVarName vars = [(TV typeVarName, INT)]
tc e@(BoolExpr _) typeVarName vars = [(TV typeVarName, BOOL)]
tc e@(AbsExpr ident expr) tvName vars = (tc expr (tvName ++ "1") (newVar:vars)) ++ [absConstr, varConstr]
	where
		termVar = TV (tvName ++ "1")
		idVar = TV (tvName ++ "0")
		varName = TV ("v" ++ tvName)
		newVar = (ident, varName)
		varConstr = (idVar, varName)
		absConstr = (TV tvName, Func idVar termVar)
tc e@(IExpr _) typeVarName vars = case lookup e vars of
	Just t -> [(TV typeVarName, t)]
	Nothing -> error "Not a closed term"
tc e@(ApExpr e1 e2) tvName vars = [apConstr] ++ e2Constrs ++ (tc e1 (tvName ++ "0") vars)
	where
		t1Var = TV (tvName ++ "0")
		t2Var = TV (tvName ++ "1")
		e2Constrs = tc e2 (tvName ++ "1") vars
		apConstr = (t1Var, Func t2Var (TV tvName))
tc e@(IfExpr cond e1 e2) tvName vars = newConstrs ++ condTc ++ e1Tc ++ e2Tc
	where
		exprVar = TV tvName
		condVar = TV (tvName ++ "0")
		e1Var = TV (tvName ++ "1")
		e2Var = TV (tvName ++ "2")
		newConstrs = [(condVar, BOOL), (exprVar, e1Var), (exprVar, e2Var)]
		condTc = tc cond (tvName ++ "0") vars
		e1Tc = tc e1 (tvName ++ "1") vars
		e2Tc = tc e2 (tvName ++ "2") vars
tc e@(LetExpr var e1 e2) tvName vars = newConstrs ++ (tc e2 (tvName ++ "2") (newVar:vars))
	where
		vart = TV (tvName ++ "0")
		e1Var = TV (tvName ++ "1")
		e2Var = TV (tvName ++ "2")
		newVar = (var, vart)
		newConstrs = ((TV tvName, e2Var):(vart, e1Var):(tc e1 (tvName ++ "1") vars))

parseExpr :: String -> ThrowsError Expr
parseExpr programText = case lexer programText of
	Left err -> Left err
	Right toks -> case parse pExpr "Parser" toks of
		Left err -> Left $ Parse err
		Right expr -> Right expr

pParens toParse = do
	tflTok LPAREN
	v <- toParse
	tflTok RPAREN
	return v

pExpr = do
	exprs <- many1 pSExpr
	return $ multiExpr exprs

pApExpr = do
	sExpr <- pSExpr
	expr <- pExpr
	return $ ApExpr sExpr expr


pSExpr = do
	expr <- pParens pExpr
		<|> pIExpr
		<|> pOpExpr
		<|> pNumExpr
		<|> pBoolExpr
		<|> pAbsExpr
		<|> pIfExpr
		<|> pLetExpr
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

pIfExpr = do
	tflTok IF
	cond <- pExpr
	tflTok THEN
	e1 <- pExpr
	tflTok ELSE
	e2 <- pExpr
	return $ IfExpr cond e1 e2

pLetExpr = do
	tflTok LET
	var <- pIExpr
	tflTok EQUAL
	sub <- pExpr
	tflTok IN
	e <- pExpr
	return $ LetExpr var sub e

multiExpr :: [Expr] -> Expr
multiExpr [] = error "No expressions in input"
multiExpr [e] = e
multiExpr (e1:e2:rest) = foldl ap (ap e1 e2) rest

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