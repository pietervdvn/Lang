module Languate.TypeTable.FullSuperTypeTable2MD (fstt2md) where

import StdDef
import MarkDown

import Data.Map as M hiding (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended


fstt2md	:: KindLookupTable -> TypeID -> FullSuperTypeTable -> MarkDown
fstt2md klt tid@(fqn, nm) fstt
	= title 3 ("Supertypes of "++nm++" "++sfrees (findWithDefault Kind tid klt)) ++
		if M.null fstt then parag "No supertypes"
		   else	table ["Is type","Requirements","Via","Orig type","Binding"] (fmap rows $ toList fstt)


rows	:: FullSTTKeyEntry -> [MarkDown]
rows (isA, (ifReq, via, (super, bnd)))
	= [st True isA,
		unwords (ifReq |> showReqs),
		fromMaybe (ital "Native") $ (via |> st True),
		st True super,
		show bnd
		]


sfrees	:: Kind -> MarkDown
sfrees k
	= [0..numberOfKindArgs k - 1] |> show |> ('a':) & unwords

showReqs	:: (Name, Set RType) -> MarkDown
showReqs (nm, reqs)
	= code nm ++ enclose ": {" "}" (intercal ", " (S.toList reqs |> st True |> code))
