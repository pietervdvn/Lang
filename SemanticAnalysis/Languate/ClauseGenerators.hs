module ClauseGenerators (generate) where

import StdDef
import Languate.AST
import Languate.TAST
import Languate.Signature
import Languate.TypeTools

{-- 
Generates the constructors (asADT/fromADT) and tuple-functions, as these can not be typechecked.

Generate should be called on each statement; it's results should be injected into a typechecked module.
See the more detailed explanation in FunctionGennerators
--}

generate	:: Statement -> [(Signature, [TClause])]
generate (FunctionStm func)
		= [func]
generate (ADTDefStm adt)
		= genADTFuncs adt
generate (SubDefStm subdef)
		= todos "Subdef clause generation"
generate (ClassDefStm classDef)
		= todos "ClassDef clause generation"
generate _	= []


genADTFuncs	:: ADTDef -> [(Signature, [TClause])]
genADTFuncs (ADTDef name frees _ sums)
		= concatMap (uncurry $ genADTSum name frees) $ zip nats sums

genADTSum	:: Name -> [Name] -> Int -> ADTSum -> [Function]
genADTSum adtName frees index (ADTSum constrName _ _ namesTypes)
		= genConstrDeconstr adtName frees constrName index (map snd namesTypes)

genConstrDeconstr	:: Name -> [Name] -> Name -> Int -> [Type] -> [Function]
genConstrDeconstr a b c d e
			= [genConstrFunct a b c d e, genDeconstr a b c d e]




genConstrFunct	:: Name -> [Name] -> Name -> Int -> [Type] ->  (Signature, [TClause])
genConstrFunct adtName frees constr int typs
		= let varNames 	= take (length typs) vars in
		  let patterns	= map TAssign varNames in
		  let typ	= toCurry typs $ apply adtName frees in
		  let decl	= [(constr, typ)] in
		  let args	= zipWith (\t n -> TCall [t] n) typs varNames in
		  let expr	= TApplication [apply adtName frees] (TCall [] "#asADT") (TNat int:args) in
			(Signature constr typ, [TClause patterns expr])

genDeconstr	:: Name -> [Name] -> Name -> Int -> [Type] -> (Signature, [TClause])
genDeconstr adtName frees constr int types
		= let varNames 	= take (length types) vars in
		  let pattern	= TDeconstruct "#fromADT" $ (TEval $ TNat int):map TAssign varNames in
		  let typ	= Curry $ apply adtName frees:[mayb $ tuple types] in
		  let args	= zipWith (\t n -> TCall [t] n) types varNames in
		  let expr	= TApplication [tuple types] (TCall [] "#asTuple") args in
		  let justExpr	= TApplication [mayb $ tuple types] (TCall [] "Just") [expr] in
		  let fallThr	= TCall [mayb $ tuple types] "Nothing" in
			(Signature constr typ, [TClause [pattern] justExpr, TClause [TDontCare] fallThr])
