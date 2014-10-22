module Languate.TypedLoader where

{--

This module calls the loader and semantic analysis to give you what you want for the interpreter.

--}
import qualified Bnf
import Languate.File2Package
import Languate.FQN
import Languate.Package2TypedPackage
import Languate.SymbolTable
import Languate.TypedPackage
import Languate.AST
import Languate.Precedence.Precedence
import Prelude hiding (lookup, Left, Right)
import Data.Map hiding (map)
import Normalizable


typedLoad	:: Bnf.World -> FQN -> FilePath -> IO TypedPackage
typedLoad world fqn@(FQN fqpn _ _) path
	=	do	package	<- loadPackage' world fqn path
			let priorTable	= buildPrecTable $ elems package
			return $ normalizePackage $ typeCheck priorTable fqpn package



typeCheck	:: PrecedenceTable -> FQPN -> Map FQN Module -> TypedPackage
typeCheck priorTable fqpn
		= buildTyped fqpn priorTable
