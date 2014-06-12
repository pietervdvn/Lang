module Bnf.Loader.Check (check, check') where

import Bnf.Converter (Error, Warning, Errors, ErrorMsg, WarningMsg, Message)
import qualified Data.Set as S
import Data.Map hiding (map, filter, foldr, null, lookup)
import qualified Data.Map as M
import Bnf.BNF
import Bnf.FQN
import Bnf.Meta.IOModule
import Bnf.Loader.ExportCalculator
import Bnf.ParseTree
import Control.Monad.Writer
import Data.List (nub)
import Data.Maybe
import Data.Tuple (swap)
import StdDef
import System.FilePath
import Control.Arrow

{--

This module checks if an IOModule qualifies as valid. It accumulates errors just like the parser.
There are quite some hoops to jump through

1 Filename should match module name
2 All imported modules should be resolved -- done by the loader which crashes
3 Names in the hiding/showing list should be exported by that module
4 No ambiguous imports should exist (importing rule 'int' from both Primitives and Arithmetic) -- if they don't point to the same thing of course!
5 No module should be imported twice (warning)
6 All calls in expressions should be resolved
7 No rules should be declared twice
--}



-- performs all needed checks, throws an error if something is wrong with a descriptive error message
check'	:: Map FQN Exports -> FilePath -> IOModule -> Module -> Module
check' context path iom m	
	=  case check context path iom m of
		(m,[])		-> m
		(m,errors)	-> error $ show errors

-- should be called on each module
-- Context (what each module **really** exports) ->  The IOModule and module that should be checked -> a list of errors
-- performs all checks that are needed
check	:: Map FQN Exports -> FilePath -> IOModule -> Module -> (Module, Errors)
check context path iom m
	= runWriter (chck context path iom m)


chck	:: Map FQN Exports -> FilePath -> IOModule -> Module -> Writer Errors Module
chck context path iom m
	= do	checkFileName (getFqn iom) path
		checkImports context iom
		checkExpressions iom m
		checkDoubleDefines iom
		return m


-- 1) FileName should check module name
checkFileName	:: FQN -> FilePath -> Writer Errors ()
checkFileName fqn@(FQN dirs name) path
	= do	let filename 	= takeBaseName path
		when (name /= filename) $ addErr fqn (0,0) "FileName" $ "The FileName " ++ show filename++" does not match the modulename "++show name
		let expected 	= foldr (\dir acc -> dir ++ "/" ++ acc) "" dirs
		let real	= dropFileName path
		let same	= and $ zipWith (==) (reverse expected) (reverse real)
		when (not same)	$ addErr fqn (0,0) "FileName" $ "The filepath "++show real ++" does not match the expected filepath "++show expected
		

-- Checks all the import stuff!
checkImports	:: Map FQN Exports -> IOModule -> Writer Errors ()
checkImports context iom@(IOM fqn _ imports _)
		= do	checkAmbigueImports context iom
			checkStatedExists context fqn imports
			checkDoubleImports fqn imports
			

-- 3) checks wether everything stated in black/whitelists does exist within the module
checkStatedExists	:: Map FQN Exports -> FQN -> [IOImport] -> Writer Errors ()
checkStatedExists ctx fqn
			=  mapM_ (checkStatedExist fqn ctx)

checkStatedExist	:: FQN -> Map FQN Exports -> IOImport -> Writer Errors ()
checkStatedExist fqn ctx (IOImport imp _ _ terms pos)
			= do	let exports	= fromJust $ M.lookup imp ctx	:: Exports
				let names	= map fst terms			:: [Name]
				let exported	= map snd $ S.toList exports	:: [Name]
				let unknown	= filter (not . (`elem` exported)) names  :: [Name]
				when (not $ null unknown) $ addErr fqn pos "Import: unresolved" $
					"The terms "++show unknown++" are not exported by "++ show imp
				return ()

checkAmbigueImports	:: Map FQN Exports -> IOModule -> Writer Errors ()
checkAmbigueImports ctx iom@(IOM fqn _ imps _)
			=  do	let all	= map (\imp -> (imp, importOne' imp ctx)) imps
				-- all contains the raw imports in a (ioimport, [name, fqn]) fashion
				-- we can scrub it four doubles now
				let ambigue	= ambigueImports all
				when (not $ null ambigue) $ addErr fqn (0,0) "Ambigue import" $
					"Some terms can refer to multiple declarations: "++ prep ambigue

prep		:: [(Name, [IOImport])] -> String
prep 		=  foldr (\toPrep acc -> "\n\t"++ prep' toPrep ++ acc) ""


prep'		:: ( Name, [IOImport]) -> String
prep' (nm, imps)
		= show nm ++ " is exported by "++( init $ foldr 
			(\(IOImport fqn _ _ _ pos) acc ->  show fqn++" "++show pos++","++acc) "" imps )


ambigueImports	:: [(IOImport, [(Name, FQN)])] -> [( Name, [IOImport])]
ambigueImports all
		=  let realAmb	= amb $ concatMap snd all in
		   let dict	= map swap $ expand $ map (fmap (map fst)) all :: [(Name, IOImport)] in
		   let cleaned	= filter ((`elem` realAmb) . fst) dict in
		   deexpand cleaned 


-- real dubble names
amb		:: [(Name, FQN)] -> [Name]
amb		=  dubbles . map fst . nub

-- ["a",[1,2,3]] -> [["a",1], ["a",2]]
expand		:: [(a,[b])] -> [(a,b)]
expand ls	=  do	(a, bs)	<- ls
			b	<- bs
			return (a,b)	


deexpand	:: Eq a => [(a,b)] -> [(a,[b])]
deexpand ls	=  let as = nub $ map fst ls in
			map (\a -> (a, forWhich a ls)) as

forWhich	:: Eq a => a -> [(a,b)] -> [b]
forWhich a	=  map snd . filter ((==) a . fst)

-- 5 generates a warning if the same file is imported twice. (Not so for one which exports and another one which imports)
checkDoubleImports	:: FQN ->  [IOImport] -> Writer Errors ()
checkDoubleImports fqn imps
		= do 	let duplicates 	= nub $ dubbles $ map (\(IOImport fqn pub _ _ _) -> (fqn, pub)) imps
			let all		= map (\(IOImport fqn _ _ _ pos) -> (fqn, pos)) imps
			when (not $ null duplicates) $ addWarn fqn (snd $ all !! 1) "Duplicate imports" $
				"Some imports are done multiple times: "  ++
				(foldr (\(fqn, _) acc -> show fqn ++ " is imported on "++ (show $ map snd $ filter ((==) fqn . fst) all) ++ acc) "" duplicates)
			

-- 7
checkDoubleDefines	:: IOModule -> Writer Errors ()
checkDoubleDefines iom	=  do	let dubbleNames	= dubbles $ map getRuleName $ getRules iom
				when (not $ null dubbleNames) $ addErr (getFqn iom) (0,0)
					"Double declare" $ "Some rules are declared multiple times: "++show dubbleNames


checkExpressions	:: IOModule -> Module -> Writer Errors ()
checkExpressions (IOM fqn _ _ rules) (Module local imported)
		= do	let impNames	= map fst $ toList imported
			let locNames	= map fst $ toList local
			let known	= S.fromList $ locNames ++ impNames
			mapM_ (checkExpression fqn known) rules

-- 6: checks if each call is resolved. E.G. if rule "a" is called, that we know what we mean with that
checkExpression	:: FQN -> S.Set Name -> IORule -> Writer Errors ()
checkExpression fqn known (IORule name expr _ _ pos)
			= do	let unknown	= nub $ unknownCalls known expr
				when (not $ null unknown) $ addWarn fqn pos "Unresolved call" $
					"The declaration of "++name++" has "++
					(if length unknown == 1 then " an unresolved call: " else " some unresolved calls: ") ++ show unknown

-- gives a list of unkown calls
unknownCalls	:: S.Set Name -> Expression -> [Name]
unknownCalls known (Call name)	= if name `S.member` known then [] else [name]
unknownCalls known (Token e)	= unknownCalls known e
unknownCalls known (Opt e)	= unknownCalls known e
unknownCalls known (Star e)	= unknownCalls known e
unknownCalls known (More e)	= unknownCalls known e
unknownCalls known (Choice e)	= concatMap (unknownCalls known) e
unknownCalls known (Seq e)	= concatMap (unknownCalls known) e
unknownCalls known (Set e)	= concatMap (unknownCalls known) e
unknownCalls known (And e ands)	= unknownCalls known e ++ concatMap (unknownCalls known . fst) ands
unknownCalls _ _		= []

addErr		:: FQN -> Position -> Name -> String -> Writer Errors ()
addErr fqn pos name msg
		=  tell $ (:[]) $ Right $ err fqn pos name msg

addWarn		:: FQN -> Position -> Name -> String -> Writer Errors ()
addWarn fqn pos name msg
		=  tell $ (:[]) $ Left $ err fqn pos name msg

err		:: FQN -> Position -> Name -> String -> Error
err fqn pos name msg
		= (rinfo fqn pos name, msg)

warn		:: FQN -> Position -> Name -> String -> Error
warn fqn pos name msg
		= (rinfo fqn pos name, msg)

rinfo			:: FQN -> Position -> Name -> RuleInfo
rinfo fqn pos name	= (fqn, name, coor pos)

coor			:: Position -> Coor
coor (line, col)	= (0, line, col)

dubbles		:: Eq a => [a] -> [a]
dubbles []	=  []
dubbles (a:as)	=  (if (a `elem` as) then (a:) else id) $ dubbles as
