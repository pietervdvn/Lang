module Languate.FunctionGenerators (generate) where

{--
Some constructs automatically generate functions, e.g. an ADT-definition defines constructors etc...
These 'extra' statements are generated here. Note that these are the functions are meant to be injected into the module **before** the typecheck. This is because some functions which are defined here (esp constructors) are called upon by the other functions.

It is a sad fact of life that exactly those important constructors **can not** be typechecked (as they use 'asADT', which is why the sister module 'ClauseGenerators' generates these for injection **after** the typecheck. The constructors are removed out of the module on the moment of the typecheck.

--}

import StdDef
import Languate.AST
import Languate.Signature
import Languate.TAST
import Languate.TypeTools

{- generates all the functions, based on a statement. Each generated function is accompanied by the statement that generated it-}
generate	:: Statement -> [(Statement, Function)]
generate stm@(FunctionStm func)
		= [(stm, func)]
generate (ADTDefStm adt)
		= genADTFuncs adt
generate (SubDefStm subdef)
		= todos "Subdef function generation"
generate (ClassDefStm classDef)
		= todos "ClassDef function generation"
generate _	= []


genADTFuncs	:: ADTDef -> [(Statement, Function)]
genADTFuncs stm@(ADTDef name frees _ [sum])
		= zip (repeat stm) genOneADTSum name frees 0 sum
genADTFuncs stm@(ADTDef name frees _ sums)
		= zip (repeat stm) concatMap (uncurry $ genADTSum name frees) $ zip nats sums


-- used for an ADT-definition which has just one sum constructor
-- makes: constructor function; deconstructor function to maybe; deconstructor function; named functions for lensing 
genOneADTSum	:: Name -> [Name] -> Int -> ADTSum -> [Function]
genOneADTSum adtName frees index (ADTSum constrName _ _ [])
		= genConstrDeconstr adtName frees constrName index []
genOneADTSum adtName frees index (ADTSum constrName _ _ namesTypes)
		= let types = map snd namesTypes in
			genConstrDeconstr adtName frees constrName index types ++
			[genDetDeconstr adtName frees constrName types]

genADTSum	:: Name -> [Name] -> Int -> ADTSum -> [Function]
genADTSum adtName frees index (ADTSum constrName _ _ namesTypes)
		= genConstrDeconstr adtName frees constrName index (map snd namesTypes)

genConstrDeconstr	:: Name -> [Name] -> Name -> Int -> [Type] -> [Function]
genConstrDeconstr a b c d e
			= [genConstrFunct a b c d e, genDeconstr a b c d e]

-- for adts
genConstrFunct	:: Name -> [Name] -> Name -> Int -> [Type] -> Function
genConstrFunct adtName frees constr int typs
		= let varNames 	= take (length typs) vars in
		  let docStr	= "Constructor '"++constr++"' for the ADT '"++adtName++"'" in
		  let typ	= toCurry typs $ apply adtName frees in
		  let decl	= [(constr, typ)] in
		  let pattern	= map Assign varNames in
		  let expr	= Seq $ [BuiltIn "asADT", Nat int]++map Call varNames in
			Function docStr Public decl [{-no laws-}] [Clause pattern expr]

genDeconstr	:: Name -> [Name] -> Name -> Int -> [Type] -> Function
genDeconstr adtName frees constr int types
		= let varNames 	= take (length types) vars in
		  let docStr	= "Deconstructor '"++constr++"' for the ADT '"++adtName++"'" in
		  let typ	= Curry $ apply adtName frees:[mayb $ tuple types] in
		  let decl	= [(constr, typ)] in
		  let pattern	= Deconstruct "#fromADT" $ (Eval $ Nat int):map Assign varNames in
		  let expr	= Seq [Call "Just", Seq $ BuiltIn "asTuple":map Call varNames] in
		  let fallThr	= Clause [DontCare] $ Call "Nothing" in
			Function docStr Public decl [{-no laws-}] [Clause [pattern] expr, fallThr]

-- used for types with only one constructor. adt -> (a,b,c,...)
genDetDeconstr	:: Name -> [Name] -> Name -> [Type] -> Function
genDetDeconstr adtName frees constr types
		= let varNames 	= take (length types) vars in
		  let docStr	= "Deterministic deconstructor '"++constr++"' for the ADT '"++adtName++"' which has only one constructor" in
		  let typ	= Curry $ apply adtName frees:[tuple types] in
		  let decl	= [(constr, typ)] in
		  let pattern	= Deconstruct "#fromADT" $ map Assign varNames in
		  let expr	= Seq $ Call "#asTuple":map Call varNames in
			Function docStr Public decl [{-no laws-}] [Clause [pattern] expr]


