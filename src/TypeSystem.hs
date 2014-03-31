module TypeSystem(
	inferType,
	TypeScheme(T, C),
	Type(INT, BOOL, TypeVar, Func)) where

import ErrorHandling
import Lexer
import Parser

data TypeScheme
	= T Type
	| C Type Type
	deriving (Eq, Show)

data Type
	= INT
	| BOOL
	| TypeVar String
	| Func Type Type
	deriving (Eq)

instance Show Type where
	show = showType

showType :: Type -> String
showType INT = "Int"
showType BOOL = "Bool"
showType (TypeVar name) = name
showType (Func t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

type Sub = [(Type, Type)]

inferType :: [(Expr, Type)] -> String -> ThrowsError (Sub, TypeScheme)
inferType context exprStr = case parseExpr exprStr of
	Left err -> Left err
	Right expr -> infType context expr

infType :: [(Expr, Type)] -> Expr -> ThrowsError (Sub, TypeScheme)
infType _ (NumExpr _) = Right ([], T INT)
infType _ (BoolExpr _) = Right ([], T BOOL)
infType context ident@(IExpr _) = lookupType context ident
infType context op@(OpExpr _) = lookupType context op
infType context abstr@(AbsExpr _ _) = Right ([], T BOOL)

lookupType :: [(Expr, Type)] -> Expr -> ThrowsError (Sub, TypeScheme)
lookupType context e = case lookup e context of
	Just t -> Right ([], T t)
	Nothing -> Left $ TypeErr $ "Error " ++ show (position e) ++ "no type in context for " ++ show e

