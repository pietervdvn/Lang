module Languate.Order (PriorityTable, Call (Expr, FCall), InfixMode (Left, Right), Priority, asCall) where

{--

This module implements 

--}
import StdDef
import Languate.AST
import Data.Map (Map, findWithDefault)
import Control.Monad.Reader
import Prelude hiding (Left, Right)


-- infixl +	--> 1+2+3 = (1+2)+3
-- infixr +	--> 1+2+3 = 1+(2+3)
-- infixr means the rightmost operation is used first
data InfixMode	= Left	| Right
	deriving (Show, Eq)
type Priority	= Int
data Call	= Expr Priority InfixMode Expression
		| FCall Priority InfixMode Call [Call]


instance Show Call where
	show (Expr p m e)	= show e -- ++"<"++show p++" "++show m++">"
	show (FCall _ _ c cs)	= show c++"(" ++ foldr (\e acc -> show e ++ " " ++ acc) "" cs ++ ")"

type PriorityTable	= Map String (Priority, InfixMode)



-- higher priority = more view
asCall		:: Expression -> Reader PriorityTable Call
asCall (Seq exprs)
		= do	exprs'	<- mapM asCall exprs
			merge exprs'
asCall (Operator o)
		= do	(prior, mode)	<- fmap (findWithDefault (1,Left) o) ask
			return $ Expr prior mode (Call o)
asCall e	= return $ Expr 0 Left e



-- converts stuff as 1+1*2 into 1+(1*2), by searching the highest operators, grouping the rest into a call (with a recursive merge) and applying those as args
-- Note: when multiple ops of the same precedence and same modus are used, these are considered equal (1+1-1; + infixr; - infixr => 1+(1-1) ); 
-- mixed modes give an error
merge		:: [Call] -> Reader PriorityTable Call
merge [call]	=  return call
merge calls	=  do	let priorities	= map priority calls
			let highest	= maximum priorities
			let homogenous	= all (==highest) priorities
			-- when homogenous (all same precedence, first expression is considered as the called function; the rest as arguments
			if homogenous then return $ simpleMerge highest calls
				else advancedMerge highest calls


advancedMerge	:: Priority -> [Call] -> Reader PriorityTable Call
advancedMerge p calls
		= do	calls'	<- group p calls	-- each block of adjecent, non highest priorities is converted to a single FCall
			let modes	= map mode $ filter (\c -> p == priority c) calls
			let mode	= checkModes modes
			-- if the mode is right, start from the right. 
			-- e.g. priority 15. Situations: [call 10, call 15] -> [Call 15 [Call 10]]  (postfix operator applied on previous thing
			-- [call 10, call 20, call 10]	-> [call 19 [ call 10, call 10]] (infix op applied on both arguments)
			-- [call 15, call 10, call 15, call 10] -> [call 15, call 14 [call 10, call 10]] -> [call 14 [call 14, [call 10, call 10]]]  (infix op applied on last two arguments, applied again)
			-- priority is decreased with one (it doesn't matter anyway)
			return $ if mode == Right then mergeRight p calls'
				else mergeLeft p calls'


mergeRight	:: Priority -> [Call] -> Call
mergeRight p [c]
		=  c
mergeRight p [c,c']
	| priority c == p
		= error $ errMsg "Prefix" "right" ++ show c -- prefix, not allowed
	| otherwise
		= FCall p Right c' [c]	-- normal postfix usage
mergeRight p (c:c':cs)
	| priority c' == p
		= FCall p Right c' [c, mergeRight p cs]
	| otherwise	= mergeRight p (c':c:cs)

-- 4 + 5 + 6
-- (4 +)

mergeLeft	:: Priority -> [Call] -> Call
mergeLeft p [c]	= c
mergeLeft p [c,c']
	| priority c == p	
		= error $ errMsg "Prefix" "left" ++ show c'-- prefix, not allowed
	| otherwise
		= FCall p Left c [c']	-- normal postfix usage
mergeLeft p (c1:op:c2:cs)
	| priority op == p
		= mergeLeft p $ (FCall p Left op [c1,c2]):cs
	| otherwise	= error $ "Invalid usage of a left associative operator, in expression "++show [c1,op,c2]


errMsg usage dir	= usage++ " usage of a "++dir++" associative operator is not allowed, use parentheses instead. Error on expression: "


checkModes	:: [InfixMode] -> InfixMode
checkModes (m:ms)
		= let correct	= all (m==) ms in
			if correct then m else
				error "Error in expression: mixed modes with the same precedence level. Some operators have a right-infix, some a left infix; try to change the precedence mode of your own operators"

group		:: Priority -> [Call] -> Reader PriorityTable [Call]
group _ []	=  return []
group p (c:cs)	=  do	let p'	= priority c
			if p == p' then do
				tail	<- group p cs
				return $ c:tail
				else do	let (lower, tail)	= gather p $ c:cs
					head		<- merge lower
					fmap (head:) $ group p tail


-- collects all calls in the first list until a call with equal priority is found
-- gather 5 [1,2,3,5,2] = ([1,2,3],[5,2])
gather		:: Priority -> [Call] -> ([Call], [Call])
gather p []	=  ([],[])
gather p (c:cs)
	| priority c == p 	= ([], c:cs)
	| otherwise		= let (init, tail) = gather p cs in
					(c:init, tail)


simpleMerge	:: Priority -> [Call] -> Call
simpleMerge p (c:cs)
		= FCall p Left c cs

priority	:: Call -> Priority
priority (Expr p _ _)
		= p
priority (FCall p _ _ _)
		= p


mode		:: Call -> InfixMode
mode (Expr _ m _)	=  m
mode (FCall _ m _ _)
		=  m

