module TypeSystem() where

data Type
	= TV String
	| INT
	| BOOL
	| Func Type Type
	deriving (Eq, Show)

type Sub = [(Type, Type)]

doSub :: Sub -> Type -> Type
doSub s f@(Func t1 t2) = case lookup f s of
	Just t -> t
	Nothing -> (Func (doSub s t1) (doSub s t2))
doSub s tv@(TV String) = case lookup tv s of
	Just t -> t
	Nothing -> tv
doSub _ t = t

