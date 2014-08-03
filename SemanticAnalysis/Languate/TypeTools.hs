module Languate.TypeTools where

-- Some misc usefull functions, esp for handling types. Used in clause/functiongenerators

import StdDef
import Languate.AST

toCurry		:: [Type] -> Type -> Type
toCurry [] t	=  t
toCurry types t	=  Curry $ types ++ [t]

apply		:: Name -> [Name] -> Type
apply t []	=  Normal t
apply t frees	=  Applied (Normal t) $ map Free frees

nats		:: [Int]
nats		= 0:map (+1) nats
vars		= map (('x':) . show) nats

tuple		:: [Type] -> Type
tuple		=  TupleType 

mayb		:: Type -> Type
mayb		=  Applied (Normal "Maybe") . (:[])
