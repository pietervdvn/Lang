module Languate.TypedLoader where

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
import qualified Languate.FunctionGenerators as FG
import qualified Languate.ClauseGenerators as CG
import qualified Languate.ExportBuilder as ExportBuilder

import Data.Maybe
import Data.Map hiding (map, null, lookup, foldr)


buildTyped package
	= todo


-- builds function -> type mapping of locally defined functions.
-- might contain infers
buildLocal	:: FQN -> Module -> (SymbolTable (DocString, [Clause]), SymbolTable [TClause])
buildLocal fqn mod
		-- generates constructor and OO and other functions
		=  let funcs	= concatMap FG.generate $ statements mod in
		   -- generate post-typecheck stuff (TClauses)
		   let funcs'	= checkDouble' $ map (genSign fqn) $ concatMap undouble funcs in
		   let clauses	= concatMap CG.generate $ statements mod in
			(Child Empty $ fromList funcs', Child Empty $ fromList clauses)


-- builds a graph of exports. Each module (reprs. by an fqn) can export arbitrary 'signatures' 
-- buildExports	:: Map FQN Module -> Map FQN [(FQN,Signature)]
buildExports	=  ExportBuilder.buildExports fqpn




--TODO fix with package manager
fqpn	= fromJust $ toFQPN "pietervdvn:Data"
impToFQN	:: Import -> FQN
impToFQN (Import _ names name _)
		= fromJust $ toFqn' fqpn names name


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
