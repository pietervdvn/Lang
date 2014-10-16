module Languate.TypedLoader where

{--

This module calls the loader and semantic analysis to give you what you want for the interpreter.

--}

import Languate.File2Package
import Languate.FQN
import Languate.Package2TypedPackage
import Languate.SymbolTable
import Languate.Order
import Languate.TypedPackage
import Languate.AST
import Prelude hiding (lookup, Left, Right)
import Data.Map hiding (map)
import Normalizable


typedLoad	:: FQN -> FilePath -> IO TypedPackage
typedLoad fqn@(FQN fqpn _ _) path
	=	do	package	<- loadPackage' fqn path
			return $ normalizePackage $ typeCheck fqpn package
							


typeCheck	:: FQPN -> Map FQN Module -> TypedPackage
typeCheck fqpn	= buildTyped fqpn priorTable

-- default, hardcoded prioritytable
-- TODO
priorTable	:: PriorityTable
priorTable	= fromList [("&&", (100, Left)), ("||", (99, Left)), ("!", (110, Right)), ("+",(30, Left)),("-",(30, Left)),("*",(20, Left)),("/",(20, Left)),("%",(20, Left)), ("²", (19, Right)), (".",(10,Right)), ("|", (17, Left)), ("$", (100, Left)) ]


