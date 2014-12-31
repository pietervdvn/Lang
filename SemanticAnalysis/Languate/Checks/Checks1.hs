module Languate.Checks.Checks1 where

{--

This module implements lots of usefull checks.

These checks both need the TLookupT, the TRequirementT and the subtype relationships

--}

import Data.Set


implicitReqs	:: TypeReqTable -> [(Name, Int)] -> RType -> Set RType
implicitReqs	= todo
