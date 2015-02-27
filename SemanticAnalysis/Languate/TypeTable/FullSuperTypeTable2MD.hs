module Languate.TypeTable.FullSuperTypeTable2MD (fstt2md,explanation) where

import StdDef
import MarkDown

import Data.Map as M hiding (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended



explanation	:: MarkDown
explanation	= title 3 "How to read a 'Supertypetable of T a0 a1'" ++
			[code "T a0 a1 " ++ "is the type given in "++bold "Is Type" ++ ", if the "++bold "requirements" ++ "on the free type variables are met. "++
				"This table contains always the same number of frees, but a certain supertype can demand extra requirements.",
			"The "++ bold "Via" ++ " column codes via what type this specific supertype was added. "++
				"This means that, if "++code "List"++" has supertype "++code "Collection"++", and "++code "Collection"++" has supertype "++code"Mappable"++
					", that "++code "List" ++"has the suppertype"++code "Mappable"++", which has been added via"++code "Collection"++". "++
				ital "Native" ++ "denotes that this supertype was added via the code."
			, "A "++bold "Binding"++"might have happened on this supertype. E.g "++code "List (k,v)"++"has the supertype"++code "Dict k v"++"."++
				code "Dict a0 a1"++"has the supertype"++code "Collection a0 a1"++"."++
				"So if we want to add the supertype"++code "Collection a0 a1"++"to"++code "List (k,v)"++", we have to substitute"++code "a0 --> k, a1 --> v"++
					"in the"++code"Collection a0 a1"++"example, if we want it to be correct."++
			"The "++bold "Orig Type"++"show this type before the substitution."] |> parag & concat


fstt2md	:: KindLookupTable -> TypeID -> FullSuperTypeTable -> MarkDown
fstt2md klt tid@(fqn, nm) fstt
	= title 3 ("Supertypes of "++nm++" "++sfrees (findWithDefault Kind tid klt)) ++
		if M.null fstt then parag "No supertypes"
		   else	table ["Is type","Requirements","Via","Orig type","Orig type reqs","Binding"] (toList fstt |> rows)


rows	:: FullSTTKeyEntry -> [MarkDown]
rows (isA, (ifReq, via, (super, bnd)))
	= [st True isA,
		unwords (ifReq |> showReqs),
		fromMaybe (ital "Native") (via |> st True),
		st True super,
		"Todo",
		show bnd
		]


sfrees	:: Kind -> MarkDown
sfrees k
	= [0..numberOfKindArgs k - 1] |> show |> ('a':) & unwords

showReqs	:: (Name, Set RType) -> MarkDown
showReqs (nm, reqs)
	= code nm ++ enclose ": {" "}" (intercal ", " (S.toList reqs |> st True |> code))
