module Languate.TypeTable.Bind.FastBind where

{--
This module implements bind, in a 'question driven' way.
--}



fastBind	:: Context -> RType -> RType -> Either String Binding
