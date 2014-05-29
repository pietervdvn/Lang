module Bnf.Loader.Convertor (convert) where

import Bnf.Meta.IOModule
import Bnf.BNF
import Bnf.FQN
import StdDef

import Data.Map (Map, fromList, member)
import qualified Data.Map as M
import Bnf.Loader.ExportCalculator
import Data.Maybe
import qualified Data.Set as S
import Data.List (nub)
{--

This module converts the IO-module-list, loaded by loader into a world. It solves cross-module dependencies.


This module closes the loop.
https://www.youtube.com/watch?v=gY2VADkYanQ. 

--}

convert	:: [IOModule] -> World
convert modules
	=  let context = calcExports modules in
		M.fromList $ map (\m -> (getFqn m, convertOne context m) ) modules

convertOne	:: Map FQN Exports -> IOModule -> Module
convertOne context iom
	= let imps = imports iom context in
		Module (localRules (getFqn iom) (M.keys imps) iom) imps


imports		:: IOModule -> Map FQN Exports -> Map Name FQN
imports (IOM fqn _ imports _) ctx
		=  let all 	= map (importOneCtx fqn ctx) $ checkDuplicateImports fqn imports in
			fromList $ checkOverlappingImports fqn $ nub $ concat $ all

-- e.g. "import A showing {a}" and "import A"; each module can be imported but once 
checkDuplicateImports	:: FQN ->  [IOImport] -> [IOImport]
checkDuplicateImports fqn imps
		= let dubbleImps	= dubbles $ map getFQN imps in
			if not $ null dubbleImps then
				error $ (errMsg fqn (0,0)) ++ (dubbleImpErrMsg dubbleImps)
			else imps

dubbleImpErrMsg	:: [FQN] -> String
dubbleImpErrMsg	fqns
		= "Some modules are imported more then once:" ++ 
			foldr (\fqn acc -> "\n\t"++show fqn++acc) "" fqns


-- e.g. "a" is found in both module "A" and "B", which one do we choose?
checkOverlappingImports	:: FQN ->  [(Name, FQN)] -> [(Name, FQN)]
checkOverlappingImports fqn imps
		= let dubbleNames = dubbles $ map fst imps in
		    if not $ null dubbleNames then
			error $ (errMsg fqn (0,0)) ++ (dubblesErrMsg dubbleNames imps)
			else imps


dubblesErrMsg	:: [Name] -> [(Name, FQN)] -> String
dubblesErrMsg dubbles imps
		= "Some terms are found in more then one module:"++
			foldr (\term acc -> "\n\t" ++ term++" is found in "++ 
				(show $ occursIn term imps) ++ acc) "" (nub dubbles)

occursIn	:: Name -> [(Name, FQN)] -> [FQN]
occursIn name	=  map snd . filter ((==) name . fst)


dubbles		:: Eq a => [a] -> [a]
dubbles []	=  []
dubbles (a:as)	=  (if (a `elem` as) then (a:) else id) $ dubbles as

importOneCtx	:: FQN -> Map FQN Exports -> IOImport -> [(Name, FQN)]
importOneCtx fqn context imprt
		= importOne' fqn (fromJust $ M.lookup (getFQN imprt) context) imprt

importOne'	:: FQN -> Exports -> IOImport -> [(Name, FQN)]
importOne' modName exps imp
		= importOne (check imp modName exps) exps

-- checks wether all stated items in black/whitelist exist
check		:: IOImport -> FQN -> Exports -> IOImport
check imprt@(IOImport fqn _ _ terms pos) modName exports
	= let needed	= (zip (repeat fqn)) $ map fst terms in
		if not $ and $ map (`S.member` exports) needed then
			error $ noExpErrMsg modName pos fqn ++ 
			(show $ map fst $ filter (not . (`S.member` exports)) needed)
		else imprt

noExpErrMsg	:: FQN -> Position -> FQN -> String
noExpErrMsg modName pos fqn
		= errMsg modName pos ++ 
			"In the import of "++show fqn ++ 
			": the following terms are not exported by the module:\n\t"

errMsg		:: FQN -> Position -> String
errMsg modName pos
		=  "Error in module: "++show modName ++" "++show pos++"\n\t"


-- assumes set of exports corresponds is from the module this improt is handling
importOne	:: IOImport -> Exports -> [(Name, FQN)]
importOne imprt@(IOImport fqn _ mode terms _)
		= (`zip` repeat fqn) . filter (isImported imprt) . map snd . S.toList

isImported	:: IOImport -> Name -> Bool
isImported (IOImport _ _ mode terms _) name
	= fromMaybe (mode == BlackList) (lookup name terms)


-- converts the IOrules into name --> Expression maps. Checks if each stated rule can be resolved against a given name
localRules	:: FQN -> [Name] -> IOModule -> Map Name Expression
localRules fqn imps iom	
 		=  let known = S.fromList $ (map (\(_, nm, _) -> nm) $ locallyDefined iom) ++ imps in
			fromList $ map (\r@(IORule name _ _ _ _) -> (name, checkExpr' fqn r known)) $ getRules iom

checkExpr'	:: FQN -> IORule -> S.Set Name -> Expression
checkExpr' fqn (IORule name e _ _ pos) known
		= let unknown	= checkExpr known e in
			if not $ null unknown then
				error $ (errMsg fqn pos) ++ "Some names are not resolved: "++show unknown
			else e

-- gives a list of unkown calls
checkExpr	:: S.Set Name -> Expression -> [Name]
checkExpr known (Call name)	= if name `S.member` known then [] else [name]
checkExpr known (Token e)	= checkExpr known e
checkExpr known (Opt e)		= checkExpr known e
checkExpr known (Star e)	= checkExpr known e
checkExpr known (More e)	= checkExpr known e
checkExpr known (Choice e)	= concatMap (checkExpr known) e
checkExpr known (Seq e)		= concatMap (checkExpr known) e
checkExpr known (Set e)		= concatMap (checkExpr known) e
checkExpr known (And e ands)	= checkExpr known e ++ concatMap (checkExpr known . fst) ands
checkExpr _ _			= []


tdata = [IOM  (FQN ["exampleBnf"] "Test1")  [("author",Left (Right "Pieter Vander Vennet"),(2,1)),("date",Right [Left 2013,Left 12,Left 23,Left 1,Left 18,Left 42],(3,1)),("desc",Left (Right "Testing bnf"),(4,1))] [IOImport  (FQN ["exampleBnf"] "Test2")  False BlackList [] (6,1)] [IORule "b" (Seq [Call "a"]) True False (8,2)],IOM  (FQN ["exampleBnf"] "Test2")  [("author",Left (Right "Pieter Vander Vennet"),(2,1)),("date",Right [Left 2013,Left 12,Left 23,Left 1,Left 18,Left 42],(3,1)),("desc",Left (Right "The syntaxis of tests!"),(4,1))] [IOImport  (FQN ["exampleBnf"] "Test1")  True BlackList [] (6,1)] [IORule "a" (Seq [Call "b"]) True False (8,2)]]

t	= convert tdata
