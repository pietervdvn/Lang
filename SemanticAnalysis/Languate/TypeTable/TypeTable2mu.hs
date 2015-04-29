module Languate.TypeTable.TypeTable2mu where

import StdDef
import HumanUtils
import Languate.TAST
import Languate.TypeTable
import Languate.MarkUp as Mu

import Data.Maybe

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

b	= Base

instance Documentable TypeTable where
	toDocument tt	=
		let fstts	= allSupertypes tt & M.toList |> fstt2doc tt
		    types	= knownTypes tt & S.toList |> type2doc tt in
			(typeTable2doc tt,  explanationFSTT:supertypesAny:types ++ fstts)



typeTable2doc	:: TypeTable -> Doc
typeTable2doc tt
	= let	rows	= knownTypes tt & S.toList
				|> (\tid -> [link' (showtid tid) ("Types/"++showtid tid),
						 b $ fst $ synops tt tid ])
		superTables	=  knownTypes tt & S.toList
				|> (Parag . Embed . ("Types/Supertypes/Supertypes of "++) . showtid)
				& Mu.Seq in

		doc "Type overview" "Everything we know about every type we know of" $
		Mu.Seq [titling "Known types" $ table ["Type", "Synopsis"] rows,
			titling "Supertype overview" superTables]

type2doc	:: TypeTable -> TypeID -> Doc
type2doc tt tid
	= let	(synopsis, rest) = synops tt tid
		kind	= kinds tt & M.findWithDefault Kind tid in
		doc ("Types/"++showtid tid) synopsis $
		Titling (Mu.Seq [b "Overview for ", Code $ imp $ showtid tid])
			$ Mu.Seq [Imp $ code $ show kind
				, imp synopsis, parag $ unlines rest,
				if tid == anyTypeID then Mu.Seq [] else
					Embed $ "Types/Supertypes/Supertypes of "++showtid tid]

synops	:: TypeTable -> TypeID -> (String, [String])
synops tt tid
	= let	descr	= docstrings tt & M.lookup tid & fromMaybe "" & lines
		synopsis= if null descr then "" else head descr
		rest	= tail' descr in
		(synopsis, rest)

fstt2doc	:: TypeTable -> (TypeID, FullSuperTypeTable) -> Doc
fstt2doc tt (tid, fstt)
	= doc ("Types/Supertypes/Supertypes of "++showtid tid) "" $ fstt2mu tt tid fstt

showtid (fqn, nm)
	= show fqn++"."++nm

fstt2mu	:: TypeTable -> TypeID -> FullSuperTypeTable -> MarkUp
fstt2mu tt tid@(fqn, nm) fstt
	= let	titl		= Mu.Seq [b "Supertypes of ", code $ st True $ vanillaType tt tid]
		tbl	= if M.null fstt then Parag $ emph "No supertypes" else
				table ["Is type","Requirements","Via","Orig type","Origsuper -> actualsuper binding","Via -> Current binding"]
				(M.toList fstt |> fsttKeyentry2mu)
		in
		Titling titl $ Mu.Seq [tbl, inlink $ title explanationFSTT]

fsttKeyentry2mu	:: FSTTKeyEntry -> [MarkUp]
fsttKeyentry2mu (isA, entry)
	= [b $ st True isA,
		Mu.Seq (reqs entry |> req2mu),
		fromMaybe (emph "Native") (viaType entry |> st True |> b),
		b $ st True $ origSuper entry,
		b $ show $ origBinding entry,
		fromMaybe (emph "Native") (stepBinding entry |> show |> b)
		]

req2mu	:: (Name, Set RType) -> MarkUp
req2mu (nm, reqs)
	= code $ if S.null reqs then nm else
		pars $ nm ++ ":" ++ commas (S.toList reqs |> st True)

explanationFSTT	:: Doc
explanationFSTT	= doc "Full super type table explanation" "How to read a super type table" $
	 Titling (Seq [Base "How to read a", Emph $ Seq [Base "Supertypetable of ", code "T a0 a1"] ]) $
	[ [code "T a0 a1",b "is the type given in", imp "Is Type",b ", if the", imp "requirements",b "on the free type variables are met.",
		b "This table contains always the same number of frees, but a certain supertype can demand extra requirements."],
	  [b "The ", imp "Via", b " column codes via what type this specific supertype was added. ",
		b "This means that, if ", code "List", b " has supertype ", code "Collection", b", and ",code "Collection", b " has supertype ",
		code"Mappable", b", that ", code "List", b"has the suppertype", code "Mappable", b", which has been added via",
		code "Collection", b". "],
	  [emph "Native" , b "denotes that this supertype was added via the code.",
		b "A ", imp "Binding", b"might have happened on this supertype. E.g ",
		code "List (k,v)", b"has the supertype", code "Dict k v", b".",
		code "Dict a0 a1", b"has the supertype", code "Collection a0 a1", b".",
		b"So if we want to add the supertype", code "Collection a0 a1", b"to", code "List (k,v)", b", we have to substitute",
		code "a0 --> k, a1 --> v",
		b"in the", code"Collection a0 a1", b"example, if we want it to be correct.",
		b"The ", imp "Orig Type", b"show this type before the substitution."
	]] |> Mu.Seq |> Parag & Mu.Seq


supertypesAny	:: Doc
supertypesAny	= doc "Types/Supertypes/Supertypes of pietervdvn:Data:Any.Any" "Or why this document is a paradox" $
			Seq [Base "The", code "Any",Base "-type ",emph "has",Base  "no super types, as it is the supertype of any possible type in the languate system." ]
