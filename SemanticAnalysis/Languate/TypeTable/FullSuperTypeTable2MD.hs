module Languate.TypeTable.FullSuperTypeTable2MD (fstt2md) where

import StdDef
import MarkDown

import Data.Map as M hiding (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe

import Languate.TAST
import Languate.TypeTable

fstt2md	:: TypeID -> FullSuperTypeTable -> MarkDown
fstt2md (fqn, nm) fstt
	= title 3 ("Supertypes of "++nm) ++
		if M.null fstt then parag "No supertypes"
		   else	table ["Is type","#Frees","Requirements","Binding","Via"] (fmap rows $ toList fstt)


rows	:: (RType, ([(Name,Set RType)], Binding, Maybe RType)) -> [MarkDown]
rows (isA, (ifReq, bnd, via))
	= [st True isA, show $ length ifReq,
		unwords (ifReq |> showReqs),
		show bnd,
		fromMaybe (ital "Native") $ (via |> st True) ]

showReqs	:: (Name, Set RType) -> MarkDown
showReqs (nm, reqs)
	= code nm ++ enclose ": {" "}" (intercal ", " (S.toList reqs |> st True))
