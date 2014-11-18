module Languate.AST.TypeASTUtils where

{--
This module implements utilities for type asts
--}

import Normalizable
import Data.List (intercalate)

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
nt t			= t


traverse	:: (Type -> Type) -> Type -> Type
traverse f (Applied t ts)
		= Applied (traverse f t) $ map (traverse f) ts
traverse f (Curry ts)
		= Curry $ map (traverse f) ts
traverse f (TupleType ts)
		= TupleType $ map (traverse f) ts
traverse f t	= f t


instance Show Type where
	show t	= case normalize t of
			(Curry tps) 	-> intercalate " -> " (map st tps)
			t		-> st t

st		:: Type -> String
st (Normal str)	=  str
st (Free str)	=  str
st (Applied t tps)
		=  "("++st t ++" "++ unwords (map st tps)++")"
st (Curry tps)	=  "("++intercalate " -> " (map st tps)++")"
st (TupleType tps)
		=  "(" ++ intercalate ", " (map st tps) ++")"
st Infer	= "_"


instance Show ADTDef where
	show (ADTDef name frees reqs docstr sums)
		= "-- "++docstr++"\ndata "++name++" "++show frees++" "++foldr (\f acc -> showTypeReq f++" "++acc) " " reqs ++ foldr (\s acc -> "\n\t"++show s++acc) "" sums

showTypeReq	:: TypeRequirement -> String
showTypeReq (name, t)
		=  "("++name++" in "++show t++")"

instance Show ADTSum where
	show (ADTSum nm v mc namedArgs )
		= "ADTSum "++nm++" "++show v ++ " " ++ show mc++" "++ show namedArgs

setVisibility	:: Visible -> ADTSum -> ADTSum
setVisibility vis (ADTSum nm _ comm args)
		= ADTSum nm vis comm args

instance Show Instance where
	show (Instance name t)	= "instance "++name++" "++show t

instance Show Law where
	show (Law n defs reqs e1 e2)	= "Law "++show n++" decl:"++show defs++" reqs:"++show reqs++" "++show e1++" "++show e2
	show (Example n e1 e2)	= "Law (ex) "++show n++" "++show e1++" "++show e2

instance Show Annotation where
	show (Annotation name str)	= "@ "++name++" : "++str
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

instance Show Expression where
	show	= se . normalize

se		:: Expression -> String
se (Nat i)	= show i
se (Flt flt)	= show flt
se (Chr c)	= show c
se (Seq expr)	= "("++unwords (map show expr) ++ ")"
se (Tuple exprs)= "("++intercalate ", " (map show exprs) ++ ")"
se (BuiltIn str)= '#':str
se (Cast t)	= "~("++show t++")"
se AutoCast	= "~~"
se (Call str)	= str
se (Operator str)= str
se _	= ""


ne		:: Expression -> Expression
ne (Seq [e])	= ne e
ne (Seq exprs)	= Seq $ nes exprs
ne (Tuple exprs)= Tuple $ map ne exprs
ne e		= e

nes	:: [Expression] -> [Expression]
nes	= filter (Seq [] /=) . map ne

isOperator		:: Expression -> Bool
isOperator (Operator _)	= True
isOperator _		= False

instance Normalizable Expression where
	normalize	= ne

instance Show ClassDef where
	show (ClassDef n frees reqs subC docs laws signs)
		= "class "++n ++" "++ show frees ++" in "++show subC++" "++ concatMap showTypeReq reqs ++ "---"++docs++"---"++show laws++show signs

instance Show SubDef where
	show (SubDef n frees t reqs)
		= show n ++" "++show frees ++" = " ++ show t ++ " where "++concatMap showTypeReq reqs

instance Show SynDef where
	show (SynDef n frees t treqs)	-- lookout! the t-reqs might eat you
		= show "type "++n++show frees ++ " = "++show t++" where "++concatMap showTypeReq treqs
