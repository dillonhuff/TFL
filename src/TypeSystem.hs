module TypeSystem(
	unify, doSub,
	Type(TV, INT, BOOL, Func, List)) where

import Data.List

data Type
	= TV String
	| INT
	| BOOL
	| List Type
	| Func Type Type
	deriving (Eq)

instance Show Type where
	show = showType

showType (TV name) = name
showType INT = "Int"
showType BOOL = "Bool"
showType (Func t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
showType (List t) = "[" ++ show t ++ "]"

typeVar :: Type -> Bool
typeVar (TV _) = True
typeVar _ = False

type Sub = [(Type, Type)]

unify :: [(Type, Type)] -> Sub
unify [] = []
unify (t:rest) = if (fst t == snd t)
	then unify rest
	else ((nextSub t) ++ (unify $ doSubList (nextSub t) $ (nextTerms t) ++ rest))

doSubList :: Sub -> [(Type, Type)] -> [(Type, Type)]
doSubList s pairs = map (\(x, y) -> (doSub s x, doSub s y)) pairs

doSub :: Sub -> Type -> Type
doSub s t@(TV _) = case lookup t s of
	Just subsT -> doSub (delete (t, subsT) s) subsT
	Nothing -> t
doSub s t@(Func t1 t2) = Func (doSub s t1) (doSub s t2)
doSub s t@(List t1) = List (doSub s t1)
doSub s t = t

nextSub :: (Type, Type) -> Sub
nextSub (TV n, t) = if not (elem (TV n) (var t))
	then [(TV n, t)]
	else []
nextSub _ = []

nextTerms :: (Type, Type) -> [(Type, Type)]
nextTerms (Func t1 t2, Func t3 t4) = [(t1, t3), (t2, t4)]
nextTerms (List t1, List t2) = [(t1, t2)]
nextTerms (TV n1, TV n2) = []
nextTerms (s, TV n) = [(TV n, s)] -- Reversal
nextTerms (s, t) = if (not $ typeVar s) && (not $ typeVar t)
	then error $ "Cannot substitute type " ++ show t ++ " for type " ++ show s
	else []

var :: Type -> [Type]
var t@(TV _) = [t]
var (Func t1 t2) = (var t1) ++ (var t2)
var _ = []