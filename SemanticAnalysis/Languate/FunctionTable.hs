module Languate.FunctionTable where

import StdDef

import Languate.AST (Coor)
import Languate.FQN
import Languate.TAST

import Data.Set
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

import Languate.MarkUp as Mu

{-

The function table is associated with a single module and keeps track of all known functions.

-}
data FunctionTable	= FunctionTable
	{ defined	:: Map Signature Coor -- signatures of locally defined functions, which might be private, and where they are defined
	, public	:: Set Signature --all public functions
	, known		:: Map Name [Signature]	-- all functions known within local scope
	}
	deriving (Show)
newtype FunctionTables	= FunctionTables {unpackFTS	:: Map FQN FunctionTable }

{-
Keeps the implementations of the locally defined functions
-}
data ImplementationTable
	= ImplementationTable {	imps		:: Map Signature [TClause]}
newtype ImplementationTables
	= ImplementationTables {unpackITS	:: Map FQN ImplementationTable }


type DocstringTable	= Map Signature String

funcSign2mu	:: Signature -> [MarkUp]
funcSign2mu sign	=
	[ imp $ signName sign
	, signTypes sign |> st True |> code & Mu.Seq
	, signTypeReqs sign & rTypeReqs2md]

docname fqn	= "Modules/FunctionTable/Function table for "++show fqn

functiontable2doc fqn ft
	= doc (docname fqn) ("Function overview for "++show fqn) $ functiontable2mu ft

functiontable2mu	:: FunctionTable -> MarkUp
functiontable2mu ft	=
	let header	= ["Name","Types","Requirements"]
	    contents f	= f ft & toList |> funcSign2mu
	    tableFor f	= table' header $ contents f
	    knowns	= known ft & M.toList |> (\(nm, signs) ->
				[imp nm, signs |> show |> code & Mu.Seq])
	    mu		= [ titling' "Defined" (tableFor (M.keysSet . defined))
			  , titling' "Exported" (tableFor public)
			  , titling' "Known" $ table' ["Name","Signatures"] knowns
			  ] & Mu.Seq
			in
		if isEmpty mu then notImportant "No functions are defined!" else mu

instance Documentable FunctionTables where
	toDocument	= functiontables2docs

functiontables2docs	:: FunctionTables -> (Doc, [Doc])
functiontables2docs (FunctionTables fts)
	=  let	all	= fts & M.toList
		docs	= all |> uncurry functiontable2doc
		embeds	= all |> fst |> docname |> Embed & Mu.Seq in
		(doc "Functiontable overview" "Overview of all functions in all modules" embeds,
			docs)


instance Documentable ImplementationTables where
	toDocument	= implTables2docs


implTables2docs	:: ImplementationTables -> (Doc, [Doc])
implTables2docs impDoc
		= let	all	= impDoc & unpackITS & M.toList
			docs	= all |> uncurry implTable2doc	:: [Doc]
			embeds	= docs |> title |> Embed & Mu.Seq  in
			(doc "Implementation table" "All signatures and typed clauses" embeds, docs)


implTable2doc	:: FQN -> ImplementationTable -> Doc
implTable2doc fqn it
	= let	conts	= it & imps & M.toList |> showEntry
		mu	= table ["Name","Type","TypeReqs","TClauses"] conts in
		doc ("Modules/ImplementationTable/"++show fqn) ("Signature and clause implementations for "++show fqn) $
			if Prelude.null conts then Base "No locally defined functions"
				else mu

showEntry	:: (Signature, [TClause]) -> [MarkUp]
showEntry (sign, clauses)
	= [code $ signName sign,
		signTypes sign |> st True |> code & parags',
		code $ show $ signTypeReqs sign,
		clauses |> show |> code & parags']

parags' [a]	= a
parags' mus	= parags mus

docstrings2mu	:: DocstringTable -> Doc
docstrings2mu docs
	= let	rows	= docs & M.toList |> (\(sign, doc) -> [code $ show sign, Base doc]) in
		doc "Docstringtable" "All docstrings of all functions" $
		titling "Docstrings" $ table ["Signature","Comment"] rows
