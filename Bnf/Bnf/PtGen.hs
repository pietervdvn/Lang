module Bnf.PtGen where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Map (Map, lookup, member, keys)
import Control.Monad.Trans
import Control.Monad
import Control.Arrow

import Bnf.BNF
import Bnf.FQN
import Bnf.ParseTree

import Consumer hiding (Exception)
import ConsumerL hiding (embed)
import StdDef
import Parser
import StateT
import Regex	
import Normalizable
{--

This module implements a parser, which parses text using a given rule.
--}

type Context	= (World, Module, FQN, Name)

data Exception	= RuleNotFound RuleInfo Name
		| NoFullParse RuleInfo

instance Show Exception where
	show (RuleNotFound (modul, _, _) name)	= "The rule "++name++" wasn't found in module '"++show modul++"'"
	show (NoFullParse inf)		= "We couldn't parse the full string, we were at "++show inf


data Pr	a	= P (Parser Exception a)
data Mode	= EatWS 	| LeaveWS
	deriving (Eq)
type St a	= StateT (Context, Mode) Pr a


-- forces that the complete string be parsed. Errors if this was not the case
parseFull	:: World -> FQN -> Name -> String -> Maybe (Either Exception ParseTree)
parseFull w fqn rule str
		= case parse w fqn rule str of
			Nothing		-> Nothing
			Just (Left ex)	-> Just $ Left ex
			Just (Right pt)	-> Just $ _isFull str pt

_isFull		:: String -> ParseTree -> Either Exception ParseTree
_isFull str pt	=  if getParseLength pt == length str then Right pt
			else Left $ NoFullParse $ getInf $ getLast pt

-- parses as much as possible
parse	:: World -> FQN -> Name -> String -> Maybe (Either Exception ParseTree)
parse world fqn name str
	| name == ""	=  error "You should not pass the empty string as a rulename"
	| otherwise	=  fmap (fmap (normalize . fst)) $ runPr (runstateT (p (Call name)) (_startState world fqn name)) str

-- lastParsePos	:: World -> FQN -> Name -> String -> Pos
lastParsePos world fqn name
	= runPrPos $ runstateT (p (Call name)) $ _startState world fqn name

_startState world fqn name	= (_startContext world fqn name, EatWS)
_startContext world fqn name	= goto fqn (world, error "The fqn you gave is not defined", fqn, name)

instance Monad Pr where
	return x	= P (return x)
	(>>=) (P ma) famb	= P (do	a <- ma
					let (P famb') = famb a
					famb')
runPr :: Pr c -> String -> Maybe (Either Exception c)
runPr pr	= outcomeToMaybe . snd . start (unliftP pr) '\n'

runPrPos	:: Pr c -> String -> (Int, Int, Int)
runPrPos pr str	=  let (_, _, pos, (l, c)) = fst $ start (unliftP pr) '\n' str in
			(pos, l, c)

liftP		:: Parser Exception a -> Pr a
liftP		=  P

unliftP		:: Pr a -> Parser Exception a
unliftP (P p)	= p

lft		=  lift . liftP

p		:: Expression -> St ParseTree
p (Call name)	=  do	Module local imported	<- getModule
			if name `member` local then do
				s <- get
				modify $ first $ gotoN name
				modify $ second $ const EatWS
				pt	<- p (fromJust $ lookup name local)
				put s
				return pt
			 else if name `member` imported then do
				s <- get
				let country	= fromJust $ lookup name imported
				modify $ first $ goto country	-- switch of context by going to the referenced module
				pt 	<- p (Call name) 		-- call the method in it's local place
				put s
				return pt
				else do	modify $ first $ gotoN name
					info	<- getInfo
					lft $ throw $ RuleNotFound info name
p (Token rule)	= do	inf	<- getInfo
			tree	<- p rule
			return $ T inf $ getContent tree
p (Rgx regex)	= do	pWs
			inf 	<- getInfo
			tk	<- lft $ longest $ match regex
			return $ T inf tk
p (Choice [])
		=  lft abort
p (Choice (r:rs))
		=  embed (p r) >>: p (Choice rs)
p (Opt rule)	=  p rule >>: (do	i <- getInfo
					return $ S i [] )
p (Star rule)	=  do	pts	<- pStar rule
			_seq pts
p (More rule)	=  do	head	<- p rule
			tail	<- pStar rule
			_seq $ head:tail
p (Seq rules)	=  do	pts	<- mapM p rules
			_seq pts
p (Set [rule])	= do	r	<- p rule
			_seq [r]
p (Set rules)	= do	(head, rest)	<- pOne rules
			S i tail	<- p (Set rest)
			return $ S i (head:tail)
p (And toP conditions)
		= do	conds	<- mapM (isolate . pCond) conditions	-- apply conditions in an isoloated way *before* the side effects of parsing toP
			r	<- p toP
			if and conds then return r else lft abort
p (NWs expr)	= do	(_,mode)	<- get
			modify $ second $ const LeaveWS
			pt		<- p expr
			modify $ second $ const mode
			return pt


-- parses whitespace if eatWS is activi
pWs		:: St ()
pWs		=  do	(_,mode)	<- get
			when (mode == EatWS) $ void $ lft $ longest $ match ws


_seq		:: [ParseTree] -> St ParseTree
_seq rules	=  do	i <- getInfo
			return $ S i rules

embed		:: St ParseTree -> St ParseTree
embed st	=  do	pt	<- st
			_seq [pt]

pStar		:: Expression -> St [ParseTree]
pStar rule	=  do	head	<- p rule
			tail	<- pStar rule
			return $ head:tail
		   >>: 	return []


pCond		:: (Expression, Bool) -> St Bool
pCond (rule, markedInvert)
		=  do	parser		<- emulateT $ p rule	-- parser :: Pr (ParseTree, Context)
			result		<- lft $ emulate $ unliftP parser
			let outcome = getOutcome result
			case outcome of
				Res _	-> return $ not markedInvert
				Nope	-> return markedInvert
				(Exc e)	-> lft $ throw e

pOne		:: [Expression] -> St (ParseTree, [Expression])
pOne [rule]	=  do	pt	<- p rule
			return (pt, [])
pOne (r:rs)	=  do	pt <- p r
			return (pt, rs)
		    >>: do	(pt, rest)	<- pOne rs
				return (pt, r:rest)


(>>:) a b	=  injState a b pref

pref		:: Pr a -> Pr a -> Pr a
pref a b	=  liftP ((>:) (unliftP a) (unliftP b))

getInfo		:: St RuleInfo
getInfo		=  do	coor	<- lft getCoor
			((_,_, fqn, name),_)	<- get
			return (fqn, name, coor)


getCoor		:: Parser Exception Coor
getCoor		=  do	ind	<- index
			(line, col) <- position
			return (ind, line, ind - col)	-- the minus is to compensate the behaviour of the newline counter

getModule	:: St Module
getModule	=  do	((_, m, _, _),_)	<- get
			return m


goto	:: FQN -> Context -> Context	
goto country (world, _, _, _)
	= if country `member` world then
		(world, fromJust $ lookup country world, country, error "Uh oh, should not happen. No rulename is set.")
		else error $ "You tried to load module '"++show country++"' but it wasn't found :(\n\tTry one of these: "++ show (keys world )

gotoN	:: Name -> Context -> Context
gotoN nm (w, c, fqn, _)
	= (w, c, fqn, nm)
