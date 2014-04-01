module TypeSystem() where

data Type
	= TV String
	| INT
	| BOOL
	| Func Type Type
	deriving (Eq)

instance Show Type where
	show = showType

showType (TV name) = name
showType INT = "Int"
showType BOOL = "Bool"
showType (Func t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

type Sub = [(Type, Type)]

unify :: [(Type, Type)] -> Sub
unify [] = []
unify (t:rest) = if (fst t == snd t)
	then unify rest
	else ((nextSub t) ++ (unify $ doSubList (nextSub t) $ (nextTerms t) ++ rest))

doSubList :: Sub -> [(Type, Type)] -> [(Type, Type)]
doSubList s pairs = map (\(x, y) -> (doSub s x, doSub s y)) pairs

doSub :: Sub -> Type -> Type
doSub s f@(Func t1 t2) = case lookup f s of
	Just t -> t
	Nothing -> (Func (doSub s t1) (doSub s t2))
doSub s tv@(TV _) = case lookup tv s of
	Just t -> t
	Nothing -> tv
doSub _ t = t

nextSub :: (Type, Type) -> Sub
nextSub (TV n, t) = if not (elem (TV n) (var t))
	then [(TV n, t)]
	else []
nextSub _ = []

nextTerms :: (Type, Type) -> [(Type, Type)]
nextTerms (Func t1 t2, Func t3 t4) = [(t1, t3), (t2, t4)]
nextTerms (TV n1, TV n2) = []
nextTerms (s, TV n) = [(TV n, s)] -- Reversal
nextTerms _ = []

var :: Type -> [Type]
var t@(TV _) = [t]
var (Func t1 t2) = (var t1) ++ (var t2)
var _ = []