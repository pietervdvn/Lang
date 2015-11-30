module Languate.TypeConstraint.Def where

import StdDef
import Languate.CheckUtils
import Languate.TAST
import Normalizable
import Control.Arrow

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
stc (All [])
	= "No typeconstraints! :)"
stc (All cons)
	= cons |> show & unlines

instance Normalizable TypeConstraint where
	normalize	= ntc

ntc	:: TypeConstraint -> TypeConstraint
ntc (All constrs)
	= let cleaned = (constrs |> normalize) & filter (not . isTrivialConstraint) in
		if length cleaned == 1 then head cleaned else All cleaned
ntc (Choose a b)
 | isTrivialConstraint a || isTrivialConstraint b
 	= All []
 | otherwise
 	= Choose (normalize a) (normalize b)
ntc tc	= tc


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


reqAsConstraint	:: RTypeReq -> [TypeConstraint]
reqAsConstraint reqs
	= reqs & unmerge |> first RFree |> uncurry SubTypeConstr

-- traverses the constraints and returns all leaves
allConstrs	:: TypeConstraint -> [TypeConstraint]
allConstrs (Choose a b)
	= allConstrs a ++ allConstrs b
allConstrs (All constrs)
	= constrs >>= allConstrs
allConstrs c
	= [c]
