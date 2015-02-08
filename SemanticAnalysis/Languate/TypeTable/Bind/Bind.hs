module Languate.TypeTable.Bind.Bind where

{--
This module implements a simple bind, which works on Full Super Type Tables
--}


{-
Binds t0 against t1. Gives a binding or a error message if the binding failed.

-}
bind	:: TypeTable -> Map RType [RType] -> RType -> RType -> Either String Binding
bind tt reqs t0 t1
	= do	(binding, reqs)	<- bind' tt t0 t1
		if null reqs then Right binding
			else Left $ "The type "++t0++" was not applied to enough arguments."




{-
Binds t0 against t1. Gives a list of requirements that still should be met (or a error message)

Each entry in the list represents the next free that should be found in the type application, and what requirements it needs.

e.g.

bind' tt "List" "Eq" -> Right ({}, [{Eq}])
meaning: no bindings, but the next type variable applied to List should be Eq.

-}
bind'	:: TypeTable -> Map RType [RType] -> RType -> RType ->
		Either String (Binding, [Set RType])
