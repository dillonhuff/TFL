module StackEvaluator(
	evalExpr) where

import ErrorHandling
import Parser

type Addr = Int

type Operation = StackM -> StackM

data StackM = SM {stk :: [(Addr, Expr)], dump :: [Int], heap :: [Addr], globals :: [(Expr, Operation)]}

push :: Expr -> StackM -> StackM
push e (SM stk d h g) = (SM ((head h, e):stk) ((head d + 1):(tail d)) (tail h) g)

top :: StackM -> Expr
top (SM stk _ _ _) = snd $ head stk

base :: StackM -> Expr
base (SM stk _ _ _) = snd $ last stk

pop :: StackM -> StackM
pop (SM stk d h g) = SM (tail stk) ((head d - 1):(tail d)) h g

popDump :: StackM -> StackM
popDump (SM stk d h g) = SM stk (tail d) h g

pushDump :: StackM -> StackM
pushDump (SM stk d h g) = SM stk (0:d) h g

newStackM :: StackM
newStackM = SM {stk = [], dump = [0], heap = [1..], globals = builtinOps}

evalExpr :: Expr -> Expr
evalExpr e = base $ eval newStackM e

eval :: StackM -> Expr -> StackM
eval sm e = case e of
	(ApExpr e1 _) -> eval (push e sm) e1
	(OpExpr _) -> doOp e sm
	_ -> popDump $ push e sm

doOp :: Expr -> StackM -> StackM
doOp e sm = case lookup e (globals sm) of
	Just op -> op sm
	Nothing -> error (show e ++ " is not a defined operator")

-- Functions for evaluating builtin operators
builtinOps =
	[(dummyOpExpr "-", unaryOp negative)
	,(dummyOpExpr "+", binaryOp plus)
	,(dummyOpExpr "*", binaryOp times)
	,(dummyOpExpr "/", binaryOp divide)
	,(dummyOpExpr "~", unaryOp boolNot)
	,(dummyOpExpr "&&", binaryOp boolAnd)
	,(dummyOpExpr "||", binaryOp boolOr)
	,(dummyOpExpr "==", binaryOp eq)
	,(dummyOpExpr "<=", binaryOp lte)
	,(dummyOpExpr ">=", binaryOp gte)
	,(dummyOpExpr "<", binaryOp lt)
	,(dummyOpExpr ">", binaryOp gt)]

binaryOp :: (Expr -> Expr -> Expr) -> StackM -> StackM
binaryOp op sm = newStack
	where
		smArg1Evaled = evalArg sm
		arg1 = top smArg1Evaled
		smArg2Evaled = evalArg $ pop smArg1Evaled
		arg2 = top smArg2Evaled
		opRes = op arg1 arg2
		newStack = push opRes $ pop smArg2Evaled

unaryOp :: (Expr -> Expr) -> StackM -> StackM
unaryOp op sm = newStack
	where
		smArg1Evaled = evalArg sm
		arg1 = top smArg1Evaled
		opRes = op arg1
		newStack = push opRes $ pop smArg1Evaled

minusOp :: StackM -> StackM
minusOp sm = newStack
	where
		smWithArgEvaled = evalArg sm
		negNum = negative $ top $ smWithArgEvaled
		newStack = push negNum $ pop smWithArgEvaled

evalArg :: StackM -> StackM
evalArg sm = finalSM
	where
		arg = arg2 $ top sm
		smWithNewDump = pushDump $ pop sm
		finalSM = popDump $ eval smWithNewDump arg

-- Arithmetic operators
negative :: Expr -> Expr
negative e = dummyNumExpr ((-1) * numVal e)

plus :: Expr -> Expr -> Expr
plus e1 e2 = dummyNumExpr (numVal e1 + numVal e2)

times :: Expr -> Expr -> Expr
times e1 e2 = dummyNumExpr (numVal e1 * numVal e2)

divide :: Expr -> Expr -> Expr
divide e1 e2 = dummyNumExpr (div (numVal e1) (numVal e2))

-- Boolean operators
boolNot :: Expr -> Expr
boolNot e = dummyBoolExpr (not $ boolVal e)

boolAnd :: Expr -> Expr -> Expr
boolAnd e1 e2 = dummyBoolExpr (boolVal e1 && boolVal e2)

boolOr :: Expr -> Expr -> Expr
boolOr e1 e2 = dummyBoolExpr (boolVal e1 || boolVal e2)

-- Comparison operators
eq :: Expr -> Expr -> Expr
eq e1 e2 = dummyBoolExpr (numVal e1 == numVal e2)

lte :: Expr -> Expr -> Expr
lte e1 e2 = dummyBoolExpr (numVal e1 <= numVal e2)

gte :: Expr -> Expr -> Expr
gte e1 e2 = dummyBoolExpr (numVal e1 >= numVal e2)

gt :: Expr -> Expr -> Expr
gt e1 e2 = dummyBoolExpr (numVal e1 > numVal e2)

lt :: Expr -> Expr -> Expr
lt e1 e2 = dummyBoolExpr (numVal e1 < numVal e2)