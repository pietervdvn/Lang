module Bnf.BNF (Module (Module), World, Expression (Rgx, NWs, Token, Call, Opt, Star, Choice, More, Seq, Set, And), tokenize, isEmpty, ws) where

import Bnf.FQN
import Data.Map hiding (map, filter, foldr)
import qualified Data.Map as Map
import StdDef
import Regex
import Regex.Def (startsWithWS)
import Normalizable
import Control.Arrow
{--

This module implements the basic data structures that a BNF needs to represents it info.

--}

data Module	= Module (Map Name Expression) (Map Name FQN) --Startrule, allrules defined locally, all rules (that are imported) mapped on their context
type World	= Map FQN Module

ws		= regex "[ \t]*"

data Expression	= Rgx Regex	-- A simple regex which is parsed and given as token (with name that of the current rule)
		| NWs Expression	-- Indication that no ws should be parsed in the following expression
		| Token Expression	-- The rule is tokenized (with name the current rule)
		| Call Name	-- call another rule
		| Opt Expression	-- rule is optional
		| Star Expression	-- Rule is repeated as much as possible
		| Choice [Expression] -- choose one rule, the *first* one is preferred
		| More Expression	-- Creates sequence with at least one time given rule
		| Seq [Expression] -- parse every rule in sequence
		| Set [Expression] -- parse each rule once, in any order. E.g. {rule1 | rule2}. Note that, when both rule1 and rule2 would be valid, rule1 is preferred
		| And Expression [(Expression, Bool)] -- Parse all the rules in (from the same starting point). The first rule is included in the PT if and only if all of the resting rules have a (non empty) result.
	deriving (Show)

instance Show Module where
	show = sm

-- has got kajira's seal of approval
sm	:: Module -> String
sm (Module name2expr name2fqn)
	= showImports name2fqn ++ "\n" ++ showRules name2expr

showImports 	= concatMap (\(name, fqn) -> "import "++show fqn++" showing {"++name++"}\n") . toList
showRules 	= concatMap (\(name, expr) -> ' ':expand name++"\t::= "++show expr++"\n" ) . toList

expand	:: String -> String
expand str
	| 7 > length str	=  str++"\t"
	| otherwise		= str

						-- If a rule is reversed (false in the snd), then this rule should *not* be parsed to add the first rule to the PT
s	:: Expression -> String
s (Rgx regex)	= " \""++ show regex ++"\""
s (NWs expr)	= " %" ++ show expr
s (Token expression)	= " $"++s expression
s (Call name)	= ' ':name
s (Choice exprs)= " (" ++ sOrred " |" exprs ++" )"
s (Opt expr)	= s expr ++ "?"
s (Star expr)	= s expr ++ "*"
s (More expr)	= s expr ++ "+"
s (Seq exprs)	= " (" ++ foldr (\e acc -> s e ++ acc) "" exprs ++ " )"
s (Set exprs)	= " {"++ sOrred " |" exprs ++ " }"
s (And expr exprs)
		= s expr ++ sAnd " &" exprs


sAnd	:: String -> [(Expression, Bool)] -> String
sAnd st	= foldr (\(e,b) acc -> (if b then "!" else "") ++ s e ++ st ++ acc) "" exprs

sOrred	:: String -> [Expression] -> String
sOrred str exprs 
	= foldr (\e acc -> s e ++ str ++ acc) (s $ last exprs) (init exprs)

-- converts an expression to a token by adding token, and by checking that
tokenize	:: Expression -> Expression
tokenize	= Token



instance Normalizable Expression where
	normalize	= n

n		:: Expression -> Expression
n (Choice rs)	= seqN rs Choice
n (Seq rs)	= seqN rs Seq
n (Set rs)	= seqN rs Set
n (Opt r)	= Opt $ n r
n (Star r)	= Star $ n r
n (More r)	= More $ n r
n (And r rb)	= case filter (not . isEmpty . fst) $ map (first n) rb of
			[]	-> n r
			rest	-> And (n r) rb 
n (Token r)	= case n r of
			r@(Token _)	-> r
			r		-> Token r
n (NWs e)	= case n e of
			e@(NWs _)	-> e
			e		-> NWs e
n rule		= rule


seqN		:: [Expression] -> ([Expression] -> Expression) -> Expression
seqN rules constr
		=  case filter (not . isEmpty) $ map n rules of
			[r]	-> r
			rs	-> constr rs 

isEmpty		:: Expression -> Bool
isEmpty (Choice [])	= True
isEmpty (Seq [])	= True
isEmpty (Set [])	= True
isEmpty (And r _)	= isEmpty r
isEmpty (Token t)	= isEmpty t
isEmpty (Star t)	= isEmpty t
isEmpty (More t)	= isEmpty t
isEmpty (Opt t)		= isEmpty t
isEmpty (NWs t)		= isEmpty t
isEmpty _		= False

