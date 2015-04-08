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
	toDocument tt	= (typeTable2doc tt,  allSupertypes tt & M.toList |> fstt2doc (kinds tt) ++ [explanationFSTT])



typeTable2doc	:: TypeTable -> Doc
typeTable2doc tt
	= doc "Type overview" "Everything we know about every type we know of" $ b "hi"

fstt2doc	:: KindLookupTable -> (TypeID, FullSuperTypeTable) -> Doc
fstt2doc klt (tid@(fqn,nm), fstt)
	= doc ("Supertypes of "++show fqn++"."++nm) "" $ fstt2mu klt tid fstt

fstt2mu	:: KindLookupTable -> TypeID -> FullSuperTypeTable -> MarkUp
fstt2mu klt tid@(fqn, nm) fstt
	= let	nrOfArgs	= findWithDefault Kind tid klt & numberOfKindArgs
		tpname		= nm++" "++ unwords (take nrOfArgs defaultFreeNames)
		titl		= Mu.Seq [b "Supertypes of ", code tpname]
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
	= code $ (if S.null reqs then nm else 
		pars $ nm ++ ":" ++ commas (S.toList reqs |> st True))

explanationFSTT	:: Doc
explanationFSTT	= doc "Full super type table explanation" "How to read a super type table" $
	 titling "How to read a 'Supertypetable of T a0 a1'" $
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