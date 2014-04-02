module StackEvaluator(
	evalExpr) where

import ErrorHandling
import Parser

data StackM = SM {stk :: [Expr], depth :: Int}

newStackM :: StackM
newStackM = SM {stk = [], depth = 0}

evalExpr :: Expr -> Expr
evalExpr e = evExpr newStackM e

evExpr :: StackM -> Expr -> Expr
evExpr _ e = e