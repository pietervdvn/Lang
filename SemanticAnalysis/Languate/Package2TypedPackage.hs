module Languate.TypedLoader where

{--
Where the action happens!
Builds a typed package from a fresh-from-parsing package (loadPackage')
--}

buildTyped	:: Map FQN Module -> Map FQN (SymbolTable [TClause])
buildTyped package
	= 
