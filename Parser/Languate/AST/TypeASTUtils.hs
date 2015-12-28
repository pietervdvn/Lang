module Languate.AST.TypeASTUtils (traverse, showTypeReq, isOperator, isExpNl, setVisibility, usedTypes, freesIn, trav, normalize, topLevelConj, removeExpNl) where

{--
This module implements utilities for type asts
--}
import StdDef
import HumanUtils (intercal, pars)


import Normalizable
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Languate.AST.TypeAST

instance Normalizable Type where
	normalize	= nt


nt	:: Type -> Type
nt (Applied t [])	= nt t
nt (Applied t [t'])	= Applied (nt t) [nt t']
-- all applied types are written as ( (State Int) Bool) to make biding easier
-- > deriveBindings (m a) (State Int Bool) = [ (m, State Int), (a, Bool)
nt (Applied t ts)	= Applied (nt $ Applied t $ init ts) [nt $ last ts]
nt (Curry [t])		= nt t
nt (Curry ts)		= Curry [nt $ head ts, nt $ Curry $ tail ts ]

nt (TupleType [t])	= nt t
nt (TupleType ts)	= TupleType $ map nt ts
nt (TypeConj [t])	= t
nt (TypeConj tps)	= tps |> nt & TypeConj
nt t			= t


traverse	:: (Type -> Type) -> Type -> Type
traverse f (Applied t ts)
		= Applied (traverse f t) $ map (traverse f) ts
traverse f (Curry ts)
		= Curry $ map (traverse f) ts
traverse f (TupleType ts)
		= TupleType $ map (traverse f) ts
traverse f t	= f t

trav		:: (Type -> a) -> ([a] -> a) -> Type -> a
trav f aggr (Applied t tps)
		= let 	h	= trav f aggr t
			tail	= map (trav f aggr) tps in
			aggr (h:tail)
trav f aggr (Curry tps)
		= aggr $ map (trav f aggr) tps
trav f aggr (TupleType tps)
		= aggr $ map (trav f aggr) tps
trav f _ t	= f t


topLevelConj	:: Type -> [Type]
topLevelConj (TypeConj tps)
		= tps
topLevelConj t	= [t]

instance Show Type where
	show t	= case normalize t of
			(Curry tps) 	-> intercalate " -> " (map st tps)
			t		-> st t

st		:: Type -> String
st (Normal nms str)
		=  intercalate "." (nms ++ [str])
st (Free str)	=  str
st (Applied t tps)
		=  "("++st t ++" "++ unwords (map st tps)++")"
st (Curry tps)	=  "("++intercalate " -> " (map st tps)++")"
st (TupleType tps)
		=  "(" ++ intercalate ", " (map st tps) ++")"
st (TypeConj tps)
		= pars (tps |> st & intercalate " & ")
st DontCareType	= "_"


freesIn	:: Type -> [Name]
freesIn	= trav _freesIn concat

_freesIn		:: Type -> [Name]
_freesIn (Free a)	= [a]
_freesIn _		= []

-- calculates which types are used in the type. This way we know what types are used and might be public. Used to build the TLT
usedTypes	:: Type -> [Name]
usedTypes	= trav _usedTypes concat


_usedTypes	:: Type -> [Name]
_usedTypes (Normal [] n)
		= [n]
_usedTypes _	= []

instance Show ADTDef where
	show (ADTDef name frees reqs sums adoptions)
		= "type "++name++" "++show frees++" "++foldr (\f acc -> showTypeReq f++" "++acc) " " reqs ++ foldr (\s acc -> "\n\t"++show s++acc) "" sums
			++ (adoptions |> show & intercal " + ")

showTypeReq	:: TypeRequirement -> String
showTypeReq (name, t)
		=  "("++name++" in "++show t++")"


-- EXPRESSION UTILS --
----------------------

isOperator		:: Expression -> Bool
isOperator (Operator _)	= True
isOperator _		= False

isExpNl			:: Expression -> Bool
isExpNl (ExpNl _)	= True
isExpNl _		= False


removeExpNl		:: Expression -> Expression
removeExpNl (ExpNl _)	= Seq []
removeExpNl (Seq exprs)	= exprs |> removeExpNl & Seq
removeExpNl (Tuple exprs)
			= exprs |> removeExpNl & Tuple
removeExpNl expr	= expr

instance Normalizable Expression where
	normalize	= ne

ne		:: Expression -> Expression
ne (Seq [e])	= ne e
ne (Seq exprs)	= Seq $ nes exprs
ne (Tuple exprs)= Tuple $ map ne exprs
ne e		= e

nes	:: [Expression] -> [Expression]
nes	= filter (Seq [] /=) . map ne

instance Show Expression where
	show	= se . normalize

se		:: Expression -> String
se (Nat i)	= "Nat" ++ show i
se (Flt flt)	= "Flt" ++ show flt
se (Chr c)	= "Chr" ++ show c
se (Seq exprs)	= "("++unwords (map show exprs) ++ ")"
se (Tuple exprs)= "("++intercalate ", " (map show exprs) ++ ")"
se (BuiltIn str typeInfo)
		= '#':str++" "++show typeInfo
se (Cast t)	= "~("++show t++")"
se AutoCast	= "~~"
se (Call str)	= str
se (Operator str)= str
se (ExpNl (Just comm))
		= "---"++comm++"---"
se _	= ""


instance Show ADTSum where
	show (ADTSum nm v namedArgs )
		= "ADTSum "++nm++" "++show v ++ " " ++ show namedArgs

setVisibility	:: Visible -> ADTSum -> ADTSum
setVisibility vis (ADTSum nm _ args)
		= ADTSum nm vis args

visible2bool	:: Visible -> Bool
visible2bool Public	= True
visible2bool Private	= False

bool2visible	:: Bool -> Visible
bool2visible True	= Public
bool2visible False	= Private

instance Show Instance where
	show (Instance (nm,id) frees t reqs)	= "instance "++ intercalate "." (nm++[id])
				++ unwords frees ++ " is "++show id++" if "++ show reqs

instance Show Law where
	show (Law n defs reqs e1 e2)	= "Law "++show n++" decl:"++show defs++" reqs:"++show reqs++" "++show e1++" "++show e2
	show (Example n e1 e2)	= "Law (ex) "++show n++" "++show e1++" "++show e2

instance Show Annotation where
	show (Annotation name str)	= "@ "++name++" : "++str

instance Show PrecedenceAnnot where
	show (PrecAnnot n mod rels)
		= "@ precedence : "++n++" is "++show mod++", "++ intercalate ", " (map show rels)

instance Show PrecModifier where
	show PrecLeft	= "left"
	show PrecRight	= "right"
	show PrecPrefix	= "prefix"
	show PrecPostfix	= "postfix"

instance Show PrecRelation where
	show (PrecEQ o1 o2)	= "(" ++ o1 ++ ") = (" ++ o2 ++ ")"
	show (PrecLT o1 o2)	= "(" ++ o1 ++ ") < (" ++ o2 ++ ")"


instance Show CatDef where
	show (CatDef n frees reqs subC laws signs)
		= "cat "++n ++" "++ show frees ++" in "++show subC++" "++ concatMap showTypeReq reqs ++ show laws++show signs

instance Show SubDef where
	show (SubDef n priv frees t reqs)
		= "subtype " ++ n ++" "++show frees ++" = " ++ show priv ++ show t ++ " where "++concatMap showTypeReq reqs

instance Functor DocString where
	fmap f (DocString comm about)
		= DocString comm $ f about
