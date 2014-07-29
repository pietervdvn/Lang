module Languate.PatternTypeChecker where

{--

This module (type)checks patterns, and produces a typed closure as byproduct.

-> Double assignments
-> Invalid deconstructions
-> Mulitdontcare gets unpacked

--}

import StdDef
import State
import Data.Map (member, insert, empty, Map)
import Languate.AST
import Languate.SymbolTable
import Control.Monad
import Data.Maybe
import Normalizable

-- TODO eval is left out for a later version
data TPattern	= TAssign Name
		| TDeconstruct Name [Pattern]
		| TMulti [TPattern]
		| TDontCare
	deriving (Show)


type TypedClosure	= Map Name Type

checkPattern	:: TypeTable -> [Pattern] -> [Type] -> ([TPattern], Map Name Type)
checkPattern tt patterns types
		= let patterns'	= sanitize (length types) patterns in
			runstate (checkAll tt patterns' types) empty

-- expands multi-dontcare into the proper amount of dont-cares
sanitize	:: Int -> [Pattern] -> [Pattern]
sanitize 0 []	=  []
sanitize i (MultiDontCare:pts)
		= replicate (i - length pts) DontCare ++ pts
sanitize i (pt:pts)
		= pt:sanitize (i-1) pts


checkAll	:: TypeTable -> [Pattern] -> [Type] -> State TypedClosure [TPattern]
checkAll tt pts	tps
	| length pts == length tps
		=  zipWithM (checkOne tt) pts tps
	| otherwise
		=  error $ "Too much/too little patterns given in comparison with the given types " ++ show pts

checkOne	:: TypeTable -> Pattern -> Type -> State TypedClosure TPattern
checkOne _ (Assign name) t
		= do	st	<- get
			when (name `member` st) $ error $ "Duplicate name detected in pattern: "++name
			modify (insert name t)
			return $ TAssign name
checkOne _ (Let _ _) _
		= todos "Let expressions in patterns are not supported yet"
checkOne tt (Deconstruct function patterns) t
		= do	let funcType	= findType function tt
			when (isNothing funcType) $ error $ "Deconstruction function not found in pattern: "++function
			let (Just typs)	= funcType
			let deconstrType	= getDeconstructType t $ map normalize typs
			when (null deconstrType) $ error $ 
				"The deconstructor function "++function++
				" does not have the right type, expected function of type "++
				show (validType t)
			-- there can only be one function with this signature
			let [producedTypes]	= deconstrType
			-- producedTypes are now the types that the subpatterns should match
			-- sanitized to remove embedded multidontcares
			let patterns'	= sanitize (length producedTypes) patterns
			patterns	<- checkAll tt patterns' producedTypes
			return $ TMulti patterns
checkOne _ (DontCare) _
		= return TDontCare
checkOne tt (Multi patterns) t
		= do	patterns'	<- mapM (flip (checkOne tt) t) patterns
			return $ TMulti patterns'
checkOne _ (Eval _) _
		= todo "Eval in checkPattern"
checkOne _ MultiDontCare _
		= error "Patterns: not sanitized. This is a bug"

validType	:: Type -> Type
validType t	=  Curry [t, Applied (Normal "Maybe") [Free "(.,.,..)"]]



-- checks wether or not the type is a valid function type that can deconstruct values in a pattern
-- These functions should take exactly one argument, and return a maybe of zero or more arguments.
getDeconstructType	:: Type -> [Type] -> [[Type]]
getDeconstructType t 	= mapMaybe (matchingType t)

-- converts a valid deconstructor-function into the types it (might) return
matchingType		:: Type -> Type -> Maybe [Type]
matchingType firstType (Curry [t, Applied (Normal "Maybe") [ts]])
	| firstType == t= Just $ case ts of
					TupleType ts'	-> ts'
					t		-> [t]
	| otherwise	= Nothing
matchingType _	_	= Nothing
			



