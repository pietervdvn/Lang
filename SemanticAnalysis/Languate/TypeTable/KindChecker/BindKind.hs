module Languate.TypeTable.BindKind where

import Data.Map

{-
Binds kinds against frees when frees are encountered

-}
bindKind	:: KindLookupTable -> Kind -> RType -> Either String (Map Name Kind)
bindKind klt Kind (RTuple tps)
	= do	bound	<- mapM (bindKind klt Kind) tps
		todos "PICKUP"
