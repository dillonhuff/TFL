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
builtinOps = [(dummyOpExpr "-", minusOp)]

minusOp :: StackM -> StackM
minusOp sm = newStack
	where
		arg = arg2 $ top sm
		newSm = pop sm
		smWithArgEvaled = eval newSm arg
		resultNum = top smWithArgEvaled
		newStack = push (negative resultNum) $ pop smWithArgEvaled

negative :: Expr -> Expr
negative e = dummyNumExpr ((-1) * numVal e)