module Languate.FunctionTable.Build where


import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.Package
import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable

import Languate.FunctionTable.Def
import Languate.FunctionTable.Utils
import Languate.FunctionTable.ModuleTraverser
import Languate.FunctionTable.BuildImplementationTables

import qualified Graphs.ExportCalculator as EC

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe

import Control.Arrow

{-
How are the function tables built?
-> We start by looking what functions are defined, with the according types
	- we check for double defined functions
	- we check that functions have an appropriate, simple kind
-> We propagate these definitions, so that each module knows what functions are visible
-- TODO: all below this line
-> We build the implementations of these defined functions
	- and typecheck those

-}

type Signaturetable	= Map FQN (Visible, Generated, Abstract)

buildFunctionTables	:: Package
				-> Map FQN TypeLookupTable
				-> Map FQN Typetable
				-> Map FQN Module
				-> Exc (Map FQN FunctionTable)
buildFunctionTables p tlts tts modules
 	= do	-- the basetables only contain declared function signatures
 		basetables	<- dictMapM (buildLocalFunctionTable tlts tts) modules
		let contains	=  basetables |> definedFuncs |> M.filter (isPublic . fst3)
					|> M.keys |> S.fromList	:: Map FQN (Set Signature)
		let fetch fqn	=  M.findWithDefault (error $ "No fqn - building FT"++show fqn) fqn contains
		let propagate'	=  propagate (importGraph' p)
		let exported	=  EC.calculateExports (importGraph p) (invertDict $ importGraph p)
					fetch propagate' :: Map FQN (Set (Signature, FQN))
		let isImported'	=  isImported (importGraph' p)
		let imported	=  EC.calculateImports (importGraph p) fetch isImported' exported	:: Map FQN (Set (Signature, [FQN]))
		let imprtTables	=  M.mapWithKey (setImported imported) basetables
		implementTables	<- dictMapM (buildImplementations p tlts) imprtTables
		return implementTables

-- Asks wether the signature can be RE-exported further. This is only the case for public imports
propagate	:: Map FQN (Map FQN Import) -> FQN -> (FQN, Signature) -> Bool
propagate importStmss self dt@(impFrom, sign)
 = let	publicImport	=  do 	(Import visibility _ _ _ _)	<- _getImpStm importStmss self impFrom
				return $ isPublic visibility
	publicImport'	= fromMaybe False publicImport
	in
	publicImport' && isImported importStmss self dt




isImported	:: Map FQN (Map FQN Import) -> FQN -> (FQN, Signature) -> Bool
isImported importStmss self (impFrom, sign)
	= fromMaybe False $
		do 	(Import _ _ _ _ restrict)	<- _getImpStm importStmss self impFrom
			return $ isAllowed restrict (signName sign)


_getImpStm	:: Map FQN (Map FQN Import) -> FQN -> FQN -> Maybe Import
_getImpStm importss self impFrom
	= do	importStms	<- M.lookup self importss
		M.lookup impFrom importStms

setImported	:: Map FQN (Set (Signature, [FQN])) -> FQN -> FunctionTable -> FunctionTable
setImported importss fqn ft
	= let	imports	= findWithDefault (error $ "No fqn - building FT set imp"++show fqn) fqn importss
				& S.toList	:: [(Signature, [FQN])]
		imports'= imports & merge |> (signName . fst &&& second concat)
				& merge	& M.fromList |> S.fromList	:: Map Name (Set (Signature, [FQN]))
		in
		ft {visibleFuncs = imports'}


buildLocalFunctionTable	:: Map FQN TypeLookupTable -> Map FQN Typetable -> FQN -> Module -> Exc FunctionTable
buildLocalFunctionTable tlts tts fqn mod
	= inside ("While building the function table for "++show fqn) $
	  do	tlt	<- M.lookup fqn tlts ? ("No tlt for "++show fqn)
	  	tt	<- M.lookup fqn tts ? ("No tt for "++show fqn)

		signs'	<- mod & statements' |+> onFirst (definedFuncSign mod tlt fqn)
				||>> unpackFirst |> concat	:: Exc [((Signature, (Visible, Generated, Abstract)), Coor)]
		signs' |> first fst |+> checkSimpleKind tt
		let signs	= signs' |> fst			:: [(Signature, (Visible, Generated, Abstract))]
		let dubble	= signs |> fst & dubbles	:: [Signature]
		let dubble'	= signs' |> first fst |> second fst & nub	-- throw away visibility and columns, then remove dubbles
					& L.filter ((`elem` dubble) . fst)
					|> (\(sign, line) -> show sign ++" "++pars ("line "++show line))
		assert (L.null dubble) ("Some functions are declared multiple times:"++indent (dubble' >>= ("\n"++)))

		let metas	= signs' |> first fst |> second (buildMeta mod)
		let err	msg	= error $ "At this point you should not need the "++msg++" tables"
		return $ FunctionTable (M.fromList (signs' |> fst)) (err "visible function") (err "implementation") (M.fromList metas)



buildMeta	:: Module -> Coor -> MetaInfo
buildMeta mod coor
	= let	(comms, docs, laws, annots)	= searchMeta mod coor
		comms'	= comms |> first (strip . stripnl)
		docs'	= docs |> first (second (strip . stripnl)) in
		MetaInfo coor laws comms' docs' annots



checkSimpleKind	:: Typetable ->  (Signature, Coor) -> Check
checkSimpleKind tt (sign, (line, _))
	= inside ("While checking kinds of function types") $
	  inside ("In the function declaration of "++show sign++", on line "++show line) $
	  do	let (rtps, reqs)	= signTypes sign
	  	let unusedFrees	= (rtps >>= freesInRT) L.\\ (reqs |> fst)
	  	let unusedReqs	= zip unusedFrees (repeat [anyType])
	  	reqKinds	<- kindOfReqs tt (reqs++unusedReqs)
	  	kinds		<-rtps |+> (try (\ e -> err e >> return Kind) . kindOf tt reqKinds)
	  	let faulty	= zip rtps kinds & L.filter ((/=) Kind . snd)
	  				|> (\(tp, kind) -> "Expecting "++plural (numberOfKindArgs kind) "more type argument"++" to "++
	  					show tp++" :: "++show kind)	:: [String]
	  	assert (L.null faulty) $ "Function types should have a normal kind:" ++indent ("\n"++unlines faulty)
