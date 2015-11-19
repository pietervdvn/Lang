module Languate.TypeConstraint.Def where

import StdDef
import Languate.CheckUtils
import Languate.TAST

data TypeConstraint
	= SubTypeConstr RType RType	-- t0 should be a subtype of t1
	| Choose TypeConstraint TypeConstraint
		-- if one set is fullfilled, this constraint is met
		-- e.g. A & B is C is met if A is C or B is C
	| All [TypeConstraint]
		-- all typeconstraints should be met
	deriving (Eq, Ord)



instance Show TypeConstraint where
	show	= stc


stc (SubTypeConstr sub super)
	= "Constraint "++show sub++" is "++indent (show super)
stc (Choose cons1 cons2)
	= "Either "++indent (show cons1)++"\nor     "++indent (show cons2)
stc (All cons)
	= cons |> show & unlines


isTrivialConstraint	:: TypeConstraint -> Bool
isTrivialConstraint (SubTypeConstr t0 t1)
			= t0 == t1
isTrivialConstraint (All [])
			= True
isTrivialConstraint _	= False

subsConstraint	:: [(Name, RType)] -> TypeConstraint -> Exc TypeConstraint
subsConstraint mapping (SubTypeConstr a b)
	= do	a'	<- subs mapping a
		b'	<- subs mapping b
		return (SubTypeConstr a' b')
subsConstraint mapping (Choose a b)
	= do	a'	<- subsConstraint mapping a
		b'	<- subsConstraint mapping b
		return (Choose a' b')
subsConstraint mapping (All conss)
	= conss |+> subsConstraint mapping |> All


subsSuper	:: [(Name, RType)] -> (RType, [TypeConstraint]) -> Exc (RType, [TypeConstraint])
subsSuper mapping (t, constrs)
	= do	t'	<- subs mapping t
		constrs'	<- constrs |+> subsConstraint mapping
		return (t', constrs')
