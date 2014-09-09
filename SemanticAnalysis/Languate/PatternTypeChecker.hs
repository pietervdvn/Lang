module Languate.PatternTypeChecker where

{--

This module (type)checks patterns, and produces a typed closure as byproduct.

Checks and crashes on
-> Double assignments
-> Invalid deconstructions
-> Mulitdontcare gets unpacked

--}

import StdDef
import State
import Normalizable

import Data.Map (member, insert, empty, Map)

import Languate.AST
import Languate.TAST
import Languate.SymbolTable
import Languate.TypeTable
import Languate.TypeChecker

import Control.Monad
import Control.Monad.Reader
import Data.Maybe




type TypedClosure	= Map Name Type

checkPattern	:: Context -> [Pattern] -> [Type] -> ([TPattern], Map Name Type)
checkPattern tt patterns types
		= let patterns'	= map normalize $ sanitize (length types) patterns in
			runstate (checkAll tt patterns' types) empty

-- expands multi-dontcare into the proper amount of dont-cares
sanitize	:: Int -> [Pattern] -> [Pattern]
sanitize 0 []	=  []
sanitize _ []	=  []
sanitize i (MultiDontCare:pts)
		= replicate (i - length pts) DontCare ++ pts
sanitize i (pt:pts)
		= pt:sanitize (i-1) pts


checkAll	:: Context -> [Pattern] -> [Type] -> State TypedClosure [TPattern]
checkAll tt pts	tps
	| length pts == length tps
		=  zipWithM (checkOne tt) pts tps
	| otherwise
		=  error $ "Too much/too little patterns given in comparison with the given types " ++ show pts

checkOne	:: Context -> Pattern -> Type -> State TypedClosure TPattern
checkOne _ (Assign name) t
		= do	st	<- get
			when (name `member` st) $ error $ "Duplicate name detected in pattern: "++name
			modify (insert name t)
			return $ TAssign name
checkOne _ (Let _ _) _
		= todos "Let expressions in patterns are not supported yet"
checkOne ctx (Deconstruct function patterns) t
		= do	let funcType	= findType function $ typeTable ctx
			let err		= error $ "Deconstruction function not found (for pattern): "++function
			let typs	= fromMaybe err funcType
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
			patterns	<- checkAll ctx patterns' producedTypes
			return $ TDeconstruct function patterns
checkOne _ (DontCare) _
		= return TDontCare
checkOne ctx (Multi patterns) t
		= do	patterns'	<- mapM (flip (checkOne ctx) t) patterns
			return $ TMulti patterns'
checkOne ctx (Eval e) t
		= do	let texp	= runReader (typeCheck e) ctx
			unless (t `elem` typeOf texp) $ error $ "Patterntypcheck: eval: the expression in the eval-pattern does not have the right type: it is of type "++show (typeOf texp)++" instead of the expected type "++show t
			return $ TEval texp
checkOne _ MultiDontCare _
		= error "Patterns: not sanitized. This is a bug"

validType	:: Type -> Type
validType t	=  Curry [t, Applied (Normal "Maybe") [Free "(.,.,..)"]]



-- checks wether or not the type is a valid function type that can deconstruct values in a pattern
-- These functions should take exactly one argument, and return a maybe of zero or more arguments in a tuple.
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
			



