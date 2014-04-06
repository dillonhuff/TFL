 module Parser(
	ExprDef,
	parseExprDefs,
	exprDef,
	typeOfProgram,
	Expr(IExpr, OpExpr, NumExpr, BoolExpr, AbsExpr, ApExpr, IfExpr, LetExpr, ListExpr, NILExpr),
	arg1, arg2, numVal, boolVal, sub,
	position,
	parseExpr,
	typeOfExpr,
	dummyIExpr, dummyOpExpr, dummyNumExpr, listExpr, nilExpr, tailExpr, headExpr, isNil,
	dummyBoolExpr, dummyAbsExpr, ifExpr, letExpr, ap) where

import Data.List as L
import ErrorHandling
import Lexer
import Text.Parsec
import TypeSystem

-- Used to store expression definitions
type ExprDef = (Expr, Expr)

exprDef :: [Expr] -> Expr -> ExprDef
exprDef (i:rest) e = (i, makeExprWithVars rest e)

makeExprWithVars :: [Expr] -> Expr -> Expr
makeExprWithVars [] e = e
makeExprWithVars (i:rest) e = AbsExpr i (makeExprWithVars rest e)

data Expr
	= IExpr PosTok
	| OpExpr PosTok
	| NumExpr PosTok
	| BoolExpr PosTok
	| AbsExpr Expr Expr
	| ApExpr Expr Expr
	| IfExpr Expr Expr Expr
	| LetExpr Expr Expr Expr
	| ListExpr Expr Expr
	| NILExpr
	deriving (Eq, Show)

sub :: Expr -> Expr -> Expr -> Expr
sub toSub var (ApExpr e1 e2) = ApExpr (sub toSub var e1) (sub toSub var e2)
sub toSub var (AbsExpr v e) = if var == v
	then sub toSub var e
	else (AbsExpr v (sub toSub var e))
sub toSub var (IfExpr cond e1 e2) =
	IfExpr (sub toSub var cond)
		(sub toSub var e1)
		(sub toSub var e2)
sub toSub var (ListExpr e1 e2) = ListExpr (sub toSub var e1) (sub toSub var e2)
sub toSub var (LetExpr v s e) = LetExpr v (sub toSub var s) (sub toSub var e)
sub toSub var e = if var == e
	then toSub
	else e


position :: Expr -> SourcePos
position (IExpr tok) = pos tok
position (OpExpr tok) = pos tok
position (NumExpr tok) = pos tok
position (BoolExpr tok) = pos tok
position (AbsExpr ident e) = position ident
position (ApExpr t1 t2) = position t1

numVal :: Expr -> Int
numVal (NumExpr postok) = num $ tok $ postok
numVal e = error $ show e ++ " is not a number"

boolVal :: Expr -> Bool
boolVal (BoolExpr postok) = bool $ tok $ postok
boolVal e = error $ show e ++ " is not a boolean"

arg1 :: Expr -> Expr
arg1 (ApExpr e1 _) = e1
arg1 e = error $ show e ++ " is not an application"

arg2 :: Expr -> Expr
arg2 (ApExpr _ e2) = e2
arg2 e = error $ show e ++ " is not an application"

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

listExpr :: Expr -> Expr -> Expr
listExpr e l@(ListExpr _ _) = ListExpr e l
listExpr e NILExpr = ListExpr e NILExpr

tailExpr :: Expr -> Expr
tailExpr (ListExpr _ t) = t
tailExpr e = error $ show e ++ " not valid list"

headExpr :: Expr -> Expr
headExpr (ListExpr h _) = h
headExpr e = error $ show e ++ " has no head"

nilExpr :: Expr
nilExpr = NILExpr

isNil :: Expr -> Expr
isNil NILExpr = dummyBoolExpr True
isNil _ = dummyBoolExpr False

ap :: Expr -> Expr -> Expr
ap t1 t2 = ApExpr t1 t2

typeOfProgram :: String -> ThrowsError Type
typeOfProgram exprStr = case parseExprDefs exprStr of
	Left err -> Left err
	Right parsedDefs -> Right $ typeOfProg parsedDefs

typeOfExpr :: String -> ThrowsError Type
typeOfExpr exprStr = case parseExpr exprStr of
	Left err -> Left err
	Right parsedExpr -> Right $ typeOf parsedExpr

-- TODO: Refactor to be less convoluted
typeOfProg :: [ExprDef] -> Type
typeOfProg defs = if length subs /= 0
	then doSub subs mainTV
	else error "NOOOO"
	where
		newVars = map (\(e, num) -> (fst e, TV ("t" ++ show num))) $ zip defs [0..(length defs - 1)]
		defExprs = map snd defs
		constraints = concat $ map (getTypeConstrs newVars) $ zip defExprs (map snd newVars)
		mainTV = case lookup (dummyIExpr "main") newVars of
			Just mVar -> mVar
			Nothing -> error "Program has no main function"
		subs = unify constraints

getTypeConstrs :: [(Expr, Type)] -> (Expr, Type) -> [(Type, Type)]
getTypeConstrs defVars (e, TV name) = typeConstraints defVars name e

typeOf :: Expr -> Type
typeOf expr = doSub sub (TV "t0")
	where
		constraints = typeConstraints [] "t0" expr
		sub = unify constraints

predefinedOps =
	[(dummyOpExpr "+", Func INT (Func INT INT))
	,(dummyOpExpr "*", Func INT (Func INT INT))
	,(dummyOpExpr "/", Func INT (Func INT INT))
	,(dummyOpExpr "-", Func INT INT)
	,(dummyOpExpr "==", Func INT (Func INT BOOL))
	,(dummyOpExpr "<=", Func INT (Func INT BOOL))
	,(dummyOpExpr ">=", Func INT (Func INT BOOL))
	,(dummyOpExpr "<", Func INT (Func INT BOOL))
	,(dummyOpExpr ">", Func INT (Func INT BOOL))
	,(dummyOpExpr "~", Func BOOL BOOL)
	,(dummyOpExpr "&&", Func BOOL (Func BOOL BOOL))
	,(dummyOpExpr "||", Func BOOL (Func BOOL BOOL))
	,(dummyIExpr "nil", (TV "a0"))
	,(dummyIExpr "cons", Func (TV "a0") (Func (List (TV "a0")) (List (TV "a0"))))
	,(dummyIExpr "head", Func (List (TV "a0")) (TV "a0"))
	,(dummyIExpr "tail", Func (List (TV "a0")) (List (TV "a0")))
	,(dummyIExpr "isNil", Func (TV "a0") BOOL)]

typeConstraints :: [(Expr, Type)] -> String -> Expr -> [(Type, Type)]
typeConstraints userDefined rootVarName expr = tc expr rootVarName (predefinedOps ++ userDefined)

tc :: Expr -> String -> [(Expr, Type)] -> [(Type, Type)]
tc e@(NumExpr _) typeVarName vars = [(TV typeVarName, INT)]
tc e@(BoolExpr _) typeVarName vars = [(TV typeVarName, BOOL)]
tc e@(OpExpr _) typeVarName vars = case L.lookup e vars of
	Just t -> [(TV typeVarName, t)]
	Nothing -> error $ show e ++ " is not a valid operator"
tc e@(AbsExpr ident expr) tvName vars = (tc expr (tvName ++ "1") (newVar:vars)) ++ [absConstr, varConstr]
	where
		termVar = TV (tvName ++ "1")
		idVar = TV (tvName ++ "0")
		varName = TV ("v" ++ tvName)
		newVar = (ident, varName)
		varConstr = (idVar, varName)
		absConstr = (TV tvName, Func idVar termVar)
tc e@(IExpr _) typeVarName vars = case lookup e vars of
	Just t -> [(TV typeVarName, polySub ("c" ++ typeVarName) t)]
	Nothing -> error $ show e ++ " is not defined\n" ++ show vars
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
		newConstrs = [(condVar, BOOL), (exprVar, e1Var), (e1Var, e2Var)]
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
tc NILExpr typeVarName vars = [] -- nil imposes no type restrictions

polySub :: String -> Type -> Type
polySub name (TV "a0") = (TV name)
polySub name (Func t1 t2) = (Func (polySub name t1) (polySub name t2))
polySub name (List t) = (List (polySub name t))
polySub name t = t

parseExpr :: String -> ThrowsError Expr
parseExpr programText = case lexer programText of
	Left err -> Left err
	Right toks -> case parse pExpr "Parser" toks of
		Left err -> Left $ Parse err
		Right expr -> Right expr

parseExprDefs :: String -> ThrowsError [ExprDef]
parseExprDefs program = case lexer program of
	Left err -> Left err
	Right toks -> case parse pExprDefs "Expr Def" toks of
		Left err -> Left $ Parse err
		Right defs -> Right defs

pExprDefs = do
	defs <- many pExprDef
	return defs

pExprDef = do
	tflTok DEF
	idents <- many1 pIExpr
	tflTok EQUAL
	expr <- pExpr
	return $ exprDef idents expr

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
		<|> pNilExpr
		<|> pIExpr
		<|> pOpExpr
		<|> pNumExpr
		<|> pBoolExpr
		<|> pAbsExpr
		<|> pIfExpr
		<|> pLetExpr
	return expr

pNilExpr = do
	tflTok NIL
	return NILExpr

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