module Languate.TypeTable.FullSuperTypeTable2MD (fstt2md) where

import StdDef
import MarkDown

import Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Languate.TAST
import Languate.TypeTable

fstt2md	:: TypeID -> FullSuperTypeTable -> MarkDown
fstt2md (fqn, nm) fstt
	= title 3 ("Supertypes of "++nm) ++
		if M.null fstt then parag "No supertypes"
		   else	table ["Is type","#Frees","Requirements"] (fmap rows $ toList fstt)


rows	:: (RType, [(Name,Set RType)]) -> [MarkDown]
rows (isA, ifReq)
	= [st True isA, show $ length ifReq,
		 unwords (ifReq |> showReqs)]

showReqs	:: (Name, Set RType) -> MarkDown
showReqs (nm, reqs)
	= code nm ++ enclose ": {" "}" (intercal ", " (S.toList reqs |> st True))
