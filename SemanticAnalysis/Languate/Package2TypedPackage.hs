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
import qualified Languate.FunctionGenerators as FG
import qualified Languate.ClauseGenerators as CG
import Languate.ExportBuilder

import Data.Maybe
import Data.Map hiding (map, null, lookup, foldr)
import qualified Data.Map as Map


-- returns all the data: per module (fqn) a typed symboltable (tClause) for the interpreter; a symboltable with docstrings (for the human using the interpreter), a symboltable with the original clauses (again for the human, for neat prints, and a symboltable which represents where each signature is originally defined -- humans are quite needy, aren't they?)
-- each symboltable represents **what is visible** within this module, including (cascaded) imports. 
buildTyped	:: FQPN -> Map FQN Module -> TPackage
buildTyped fqpn package
	=   let localBuild	= mapWithKey buildLocal package in
		-- builds a graph of exports. Each module (reprs. by an fqn) can export arbitrary 'signatures' 
	    let exports	= buildExports fqpn package in	-- Map FQN [(FQN, Signature)]
	    	todo


buildTModule	:: FQN -> TModule
buildTModule fqn
		=  let docstr	= todo in
			todo


-- builds function -> type mapping of locally defined functions (including some for post-typecheck-injection).
-- might contain infers in the types
buildLocal	:: FQN -> Module -> (SymbolTable (DocString, [Clause]), SymbolTable [TClause])
buildLocal fqn mod
		-- generates constructor and OO and other functions
		=  let funcs	= concatMap FG.generate $ statements mod in
		   -- generate post-typecheck stuff (TClauses)
		   let funcs'	= checkDouble' $ map (genSign fqn) $ concatMap undouble funcs in
		   let clauses	= concatMap CG.generate $ statements mod in
			(Child Empty $ fromList funcs', Child Empty $ fromList clauses)




-- extracts the locally declared signatures out of the data vomit of buildLocal
localDeclared	:: Map FQN (SymbolTable a, b) -> Map FQN [Signature]
localDeclared 	= Map.map (signatures . fst)


genSign		:: FQN -> Function -> (Signature, (DocString, [Clause]))
genSign fqn (Function doc [(name,typ)] _ imp)
		= (Signature name typ, (doc,imp))

undouble	:: Function -> [Function]
undouble (Function doc nmTypes laws imp)
		= do	(name, typ)	<- nmTypes
			return $ Function doc [(name,typ)] laws imp

checkDouble'	:: [(Signature, a)] -> [(Signature, a)]
checkDouble' ls	=  let (signs, as)	= unzip ls in
			zip (checkDouble signs) as

checkDouble	:: [Signature] -> [Signature]
checkDouble signs
		= if null $ dubbles signs then signs
			else error $ "Double signatures exist: "++ show (map show $ dubbles signs)
