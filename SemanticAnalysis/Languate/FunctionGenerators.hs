module Languate.FunctionGenerators where

{--
Some constructs automatically generate functions, e.g. an ADT-definition defines constructors etc...
--}

import Languate.AST
import Languate.Signature
import StdDef
import Languate.TAST


-- generates all the functions, based on a statement.
-- Note that this function might imply already typechecked functions too, which should be included directly
generate	:: Statement -> [Function]
generate (FunctionStm func)
		= [func]
generate (ADTDefStm adt)
		= genADTFuncs adt
generate (SubDefStm subdef)
		= todos "Subdef function generation"
generate (ClassDefStm classDef)
		= todos "ClassDef function generation"
generate _	= []


-- generates functions directly for the interpreter, these will mainly be the constructors
generateConstr	:: Name -> [Name] -> Name -> Int -> [Type] -> (Signature, [TClause])
generateConstr adtName frees constr int typs
		= let varNames 	= take (length typs) vars in
		  let typ	= toCurry typs $ apply adtName frees in
		  let sign	= Signature constr typ in
		  let pattern	= map Assign varNames in
		  let expr	= Seq $ [BuiltIn "asADT", Nat int]++map Call varNames in
			(sign, todo)



genADTFuncs	:: ADTDef -> [Function]
genADTFuncs (ADTDef name frees _ [sum])
		= genOneADTSum name frees 0 sum
genADTFuncs (ADTDef name frees _ sums)
		= concatMap (uncurry $ genADTSum name frees) $ zip nats sums


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
			Function docStr decl [{-no laws-}] [Clause pattern expr]

genDeconstr	:: Name -> [Name] -> Name -> Int -> [Type] -> Function
genDeconstr adtName frees constr int types
		= let varNames 	= take (length types) vars in
		  let docStr	= "Deconstructor '"++constr++"' for the ADT '"++adtName++"'" in
		  let typ	= Curry $ apply adtName frees:[mayb $ tuple types] in
		  let decl	= [(constr, typ)] in
		  let pattern	= Deconstruct "#fromADT" $ (Eval $ Nat int):map Assign varNames in
		  let expr	= Seq [Call "Just", Seq $ BuiltIn "asTuple":map Call varNames] in
		  let fallThr	= Clause [DontCare] $ Call "Nothing" in
			Function docStr decl [{-no laws-}] [Clause [pattern] expr, fallThr]

-- used for types with only one constructor. adt -> (a,b,c,...)
genDetDeconstr	:: Name -> [Name] -> Name -> [Type] -> Function
genDetDeconstr adtName frees constr types
		= let varNames 	= take (length types) vars in
		  let docStr	= "Deterministic deconstructor '"++constr++"' for the ADT '"++adtName++"' which has only one constructor" in
		  let typ	= Curry $ apply adtName frees:[tuple types] in
		  let decl	= [(constr, typ)] in
		  let pattern	= Deconstruct "#fromADT" $ map Assign varNames in
		  let expr	= Seq $ Call "#asTuple":map Call varNames in
			Function docStr decl [{-no laws-}] [Clause [pattern] expr]

toCurry		:: [Type] -> Type -> Type
toCurry [] t	=  t
toCurry types t	=  Curry $ types ++ [t]

apply		:: Name -> [Name] -> Type
apply t []	=  Normal t
apply t frees	=  Applied (Normal t) $ map Free frees

nats		= 0:map (+1) nats
vars		= map (('x':) . show) nats

tuple		:: [Type] -> Type
tuple ls	=  TupleType ls

mayb		:: Type -> Type
mayb		=  Applied (Normal "Maybe") . (:[])
