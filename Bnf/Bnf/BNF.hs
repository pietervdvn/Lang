module Bnf.BNF (Module (Module), World, Expression (Rgx, WsRgx, Token, Call, Opt, Star, Choice, More, Seq, Set, And), tokenize, isEmpty, ws) where

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
		| WsRgx Regex	-- a regex which parses the leading whitespace itself
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

showImports name2fqn	= concat $ map (\(name, fqn) -> "import "++show fqn++" showing {"++name++"}\n") $ toList $ name2fqn
showRules name2expr 	= concat $ map (\(name, expr) -> " "++expand name++"\t::= "++show expr++"\n" ) $ toList $ name2expr

expand	:: String -> String
expand str
	| 7 > length str	=  str++"\t"
	| otherwise		= str

						-- If a rule is reversed (false in the snd), then this rule should *not* be parsed to add the first rule to the PT
s	:: Expression -> String
s (Rgx regex)	= " \""++(show regex)++"\""
s (WsRgx regex)	= s (Rgx regex)
s (Token expression)	= " $("++s expression++")"
s (Call name)	= " "++name
s (Choice exprs)= " (" ++ sOrred exprs ++" )"
s (Opt expr)	= s expr ++ "?"
s (Star expr)	= s expr ++ "*"
s (More expr)	= s expr ++ "+"
s (Seq exprs)	= " (" ++ foldr (\e acc -> s e ++ acc) "" exprs ++ " )"
s (Set exprs)	= " {"++ sOrred exprs ++ " }"
-- TODO and

sOrred	:: [Expression] -> String
sOrred exprs = foldr (\e acc -> s e ++ " |" ++ acc) (s $ last exprs) (init $ exprs)

-- converts an expression to a token by adding token, and by checking that, if the first chars of the first regex contain whitespace, using wsregex
tokenize	:: Expression -> Expression
tokenize(Rgx rgx)
	| startsWithWS rgx	= WsRgx rgx
	| otherwise		= Rgx rgx
tokenize (Choice es)
	= Choice $ map tokenize es
tokenize (Opt e)	= Opt $ tokenize e
tokenize (Star es)	= Star $ tokenize es
tokenize (More es)	= More $ tokenize es
tokenize (Seq es)	= Seq $ map tokenize es
tokenize (Set es)	= Set $ map tokenize es
tokenize (And e es)	= And (tokenize e) (map (first tokenize) es)
tokenize e		= e



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
			(Token r)	-> r
			r		-> r
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
isEmpty _		= False
{-
$token	= "regex" -- whitespace sensitive IF the regex starts with ws
parserule	= token | more tokens "anonToken" -- non whitespace sensitive (but newline sensitive)
_localRule	= 
>initialRule	= 


= rule* === rule rule+	-- rule is parsed as much as possible (optionaly none)
= rule+
= rule?
= (ruleA ruleB) | ruleC
=== rulea ruleB | ruleC
= set {needed1 | needed2 } -- both 'set needed1'  and 'set needed2' are valid
= rule1 & rule2 : rule 1 is parsed if rule 2 if parsed, but rule 2 is NOT integrated in rule2
= rule1 & !(rule2) : rule 1 is parsed if rule 2 can not be parsed

-}
