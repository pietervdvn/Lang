module Languate.Package2TypedPackage where

{--
Where the action happens!
Builds a typed package from a fresh-from-parsing package (loadPackage'), and some other things which are needed down the way (docstrings etc...)
--}

import StdDef
import Languate.Signature
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.SymbolTable
import Languate.TypedPackage
import Languate.ModuleTypeChecker
import Languate.Order (PriorityTable)
import qualified Languate.FunctionGenerators as FG
import qualified Languate.ClauseGenerators as CG
import Languate.ExportBuilder

import Data.Maybe
import Data.Tuple
import Data.Map hiding (map, null, lookup, foldr, filter, foldr)
import qualified Data.Map as Map


-- returns all the data: per module (fqn) a typed symboltable (tClause) for the interpreter; a symboltable with docstrings (for the human using the interpreter), a symboltable with the original clauses (again for the human, for neat prints, and a symboltable which represents where each signature is originally defined -- humans are quite needy, aren't they?)
-- each symboltable represents **what is visible** within this module, including (cascaded) imports. 
buildTyped	:: FQPN -> PriorityTable -> Map FQN Module -> TPackage
buildTyped fqpn prior package
	=   let localBuildExt	= mapWithKey (buildLocalSign Public) package in
		-- localBuild: what does each package export from locally declared functions?
		-- then: build a graph of exports. Each module (reprs. by an fqn) can export arbitrary 'signatures' 
	    let exports	= buildExports fqpn package localBuildExt :: Map FQN [(FQN, Signature)] in
	    let scopes	= mapWithKey (buildScope exports) package :: Map FQN [(FQN, Signature)] in
	    let funcInfo	= Map.map buildLocal package in
	    	Map.map (buildTModule prior funcInfo) scopes


buildTModule	:: PriorityTable -> Map FQN (SymbolTable DocString, SymbolTable [Clause]) 
		-> [(FQN, Signature)] -> TModule
buildTModule priorTable funcInfo scope
		=  let (locs, docs, clauses)	= unzip3 $ reverse $ map (uncurry $ scopeData funcInfo) scope in
			-- if you find something that has the same meaning as 'clauses' and rhymes with 'clauses', please send a pull request
			-- reverse is added to put the most important signatures (the most local/last imported) last; the 'fromList' in Data.Map will then add those if duplicate entries exist
		   let (locsT, docsT, clausesT)	= (stFromList locs, stFromList docs, stFromList clauses) in
		   let tclauses		= typeCheckModule priorTable clausesT in
			TModule tclauses docsT clausesT locsT
			


scopeData	:: Map FQN (SymbolTable DocString, SymbolTable [Clause])
		->  FQN -> Signature 
		-> ((Signature, FQN),(Signature, DocString),(Signature, [Clause]))
scopeData funcInfo fqn sign
		= let (docStrST, clausesST)	
			= findWithDefault 
			(error $ "bug: Semantal/Package2Typed/scopeData: name not found: "++show fqn) 				fqn funcInfo :: (SymbolTable DocString, SymbolTable [Clause]) in
		  let docStr	= fromMaybe "" $ lookupSt sign docStrST in
		  let clauses	= fromMaybe (error $ "empty function?! semantal/pack2typedpack/scopeData"++show fqn++" : "++show sign) $ lookupSt sign clausesST in
			((sign, fqn), (sign, docStr), (sign, clauses))


-- builds the full scope, thus all fqn signatures which are visible in this fqn
buildScope	:: Map FQN [(FQN, Signature)] -> FQN -> Module -> [(FQN, Signature)]
buildScope exports fqn@(FQN fqpn _ _) modul
		= let local	= zip (repeat fqn) $ buildLocalSign Private fqn modul :: [(FQN, Signature)] in
		  let imps	= extractImports fqpn modul :: [(FQN, Visible, Restrict)] in
		  let exps	= concatMap (extractExports exports) imps in
		  local++exps
			

extractExports	:: Map FQN [(FQN, Signature)] -> (FQN, Visible, Restrict) -> [(FQN, Signature)]
extractExports exports (fqn, _, restrict)
		=  let exps	= findWithDefault [] fqn exports in
			mask' restrict exps 

-- builds function -> type mapping of locally defined functions. All functions, including private ones, are given.
buildLocal	:: Module -> (SymbolTable DocString, SymbolTable [Clause])
buildLocal mod
		-- generates constructor and OO and other functions
		=  let funcs	= concatMap FG.generate $ statements mod in
		   let funcs'	= map getData funcs :: [(Signature, (DocString, [Clause]))] in
			unzipST $ Child Empty $ fromList funcs'

-- builds all the locally declared signatures.hen visible == Public, only the public functions will be included. When visible == private, all functions are given. (This includes the restrict of the entire module)
buildLocalSign	:: Visible -> FQN -> Module -> [Signature]
buildLocalSign visibleNeeded fqn mod
		-- generates constructor and OO and other functions
		=  let funcs	= concatMap FG.generate $ statements mod in
		   -- generate post-typecheck stuff (TClauses)
		   let filteredFuncs	= filter ((<=) visibleNeeded . visibility)
					$ concatMap undouble funcs in
		   checkDouble  $ mask (exports mod)
				$ checkExportsExist fqn (exports mod) 
				$ concatMap signature filteredFuncs

getData		:: Function -> (Signature, (DocString, [Clause]))
getData (Function doc v [(name,typ)] _ imp)
		= (Signature name typ, (doc,imp))

undouble	:: Function -> [Function]
undouble (Function doc v nmTypes laws imp)
		= do	(name, typ)	<- nmTypes
			return $ Function doc v [(name,typ)] laws imp

checkDouble'	:: [(Signature, a)] -> [(Signature, a)]
checkDouble' ls	=  let (signs, as)	= unzip ls in
			zip (checkDouble signs) as

checkDouble	:: [Signature] -> [Signature]
checkDouble signs
		= if null $ dubbles signs then signs
			else error $ "Double signatures exist: "++ show (map show $ dubbles signs)

checkExportsExist	:: FQN -> Restrict -> [Signature] -> [Signature]
checkExportsExist _ (BlackList [])	signs
			= signs
checkExportsExist fqn (WhiteList nms) signs
			= let avail	= map (\(Signature nm _) -> nm) signs in
				foldr (\nm acc -> check avail nm acc) signs nms
				where 	check	:: [Name] -> Name -> a -> a
					check avail name a
					 | name `elem` avail	= a
					 | otherwise		= error $ show fqn ++" exports "++show name++" which is not defined or is private"
					
