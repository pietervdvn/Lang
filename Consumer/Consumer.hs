module Consumer where

-- a consumer consumes of a list input.
-- It can have three outcomes: Res a , the actual result, Nope, when nothing could be consumed and Exc de when an exception happened.
-- A concept of state is mixed in too.

-- This is an ugly mix of side effects; monad transformers should be used, but are somewhat more impractical to use.
import StdDef
import Prelude hiding (catch)

data Outcome e a	= Res a | Exc e | Nope
	deriving (Ord, Show, Eq)
type Message		= String

data Exception i e	= Exception (State i) Message e  -- Original stream, position where error happened, exception message, additional data (such as underlying error)
	deriving (Ord, Eq)

instance Show (Exception i e) where
	show (Exception (_, _, pos, (line, col)) msg _)	= "Exception while consuming stream at line "++show line ++ ", char "++show col++" (" ++ show pos ++ "): "++msg

type State i		= ([i], i, Pos, Position) -- each time the second i is found, the position is incremented
type Result i e a	= (State i, Outcome e a)
data Consumer i e a	= Consumer ( State i ->  Result i e a )

run			:: Consumer i e a -> State i -> Result i e a
run (Consumer f)	=  f

start			:: Consumer i e a -> i -> [i] -> Result i e a
start (Consumer f) nl ls
			=  f (ls, nl, 0, (0,0))

-- rerun takes a consumer and transforms it to (virtually) consume other input.
-- can be usefull, e.g. when tokens are consumed, (Tokens consist of content as e.g. a String)
-- Rerun can than be used to pass in the string, while still being in a token consumer
rerun			:: Consumer i e a -> State i -> Consumer j e a
rerun cons st		=  Consumer $ \ statej -> (statej, getOutcome $ run cons st)

getState		:: Result i e a -> State i
getState		=  fst

getOutcome		:: Result i e a -> Outcome e a
getOutcome		=  snd

outcomeToMaybe		:: Outcome e a -> Maybe (Either e a)
outcomeToMaybe (Nope)	=  Nothing
outcomeToMaybe (Res a)	=  Just (Right a)
outcomeToMaybe (Exc e)	=  Just (Left e)

unpack			:: Show e => Result i e a -> a
unpack res		=  _unpck $ getOutcome res
				where 	_unpck		:: Show e => Outcome e a -> a
					_unpck (Res a) = a
					_unpck (Nope)= error "No result"
					_unpck (Exc e)	= error $ show e

unpackResting		:: Result i e a -> [i]
unpackResting ((is,_,_,_),_)	=  is

unpack'			:: Result i e a -> a
unpack' res		=  _unpck $ getOutcome res
				where 	_unpck		:: Outcome e a -> a
					_unpck (Res a) = a
					_unpck Nope	= error "No result"
					_unpck (Exc _)	= error "Exception which does not instantiate show"


instance Monad (Consumer i e) where
	return a	= Consumer $ \ st -> (st, Res a)
	x >>= f		= Consumer $ \ st -> exec (run x st) f
				where 	exec			:: Result i e a -> (a -> Consumer i e b)-> Result i e b
					exec (st, Res a) fn	= run (fn a) st
					exec (st, Exc e) _	= (st, Exc e)
					exec (st, Nope)  _	= (st, Nope)

_const			:: Outcome e a -> State i -> Result i e a
_const a st		=  (st, a)

abort			:: Consumer i e a
abort			=  Consumer (_const Nope)

continue		:: Consumer i e ()
continue		=  return ()

continueIf		:: Bool -> Consumer i e ()
continueIf bool		=  if bool then continue else abort




throw			:: e -> Consumer i e a
throw msg		=  Consumer ( _const $ Exc msg)

throwIf			:: Bool -> e -> Consumer i e ()
throwIf False _		=  continue
throwIf True msg	=  throw msg

catch			:: Eq i => (e -> Consumer i e2 a) -> Consumer i e a -> Consumer i e2 a
catch handle (Consumer f)	=  Consumer $ \ st -> lookUp handle $ f st
					where 	lookUp	:: (e -> Consumer i e2 a) -> Result i e a -> Result i e2 a
						lookUp _ (st, Res a)	= (st, Res a)
						lookUp _ (st, Nope)	= (st, Nope)
						lookUp h (st, Exc e)	= run (h e) st

catchFail		::  Eq i => (Show e) => Consumer i e a -> Consumer i e2 a
catchFail		=   catch (\e -> error $ "Consuming stream failed. Message: "++show e)


-- ignore cousing error, but throw something else
rethrow			:: Eq i => e -> Consumer i e2 a -> Consumer i e a
rethrow e		=  catch (\ _ -> throw e)

resting			:: Eq i => Consumer i e [i]
resting			=  Consumer $ \ st@(is, _,_,_) -> (st, Res is)

lengthRest		:: Eq i => Consumer i e Int
lengthRest		=  Consumer $ \ st@(is, _,_,_) -> (st, Res $ length is)

done			:: Eq i => Consumer i e Bool
done			=  Consumer $ \ st@(is, _,_,_) -> (st, Res $ null is)

eosOr			:: Eq i => Consumer i e a -> Consumer i e ()
eosOr cns		=  do	d <- done
				if d then continue
					else throwAwayResult cns

throwAwayResult		:: Consumer i e a -> Consumer i e ()
throwAwayResult cns	=  do	cns
				continue

opt			:: Eq i => Consumer i e a -> Consumer i e ()
opt cns			=  (cns >> continue) >: continue

index			:: Eq i => Consumer i e Int
index			=  Consumer $ \ st@(_, _, i, _) -> (st, Res i)

-- returns a position, which is composed out of "lineNumber, line started at this chars"
position		:: Eq i => Consumer i e Position
position		=  Consumer $ \ st@(_, _, _, i) -> (st, Res i)

current			:: Eq i => Consumer i e i
current			=  Consumer tk
				where 	tk 		:: State i -> (State i, Outcome e i)
					tk st@([], _,_,_)	=  (st, Nope)
					tk st@(i:_,_,_,_)	=  (st, Res i)

state			:: Consumer i e (State i)
state			=  Consumer $ \ st -> (st, Res st)

setState		:: State i -> Consumer i e ()
setState state		=  Consumer $ const (state, Res ())

setOutcome		:: Outcome e a -> Consumer i e a
setOutcome result	= Consumer $ \ st -> (st, result)

setResult		:: Result i e a -> Consumer i e a
setResult result	=  Consumer $ const result

emulate			:: Consumer i e a -> Consumer i e (Result i e a)
emulate consumer	=  do	state	<- state
				return $ run consumer state


next			:: Eq i => Consumer i e i
next			=  Consumer tk
				where 	tk 		:: Eq i =>  State i -> (State i, Outcome e i)
					tk ([],i, ind, pos)	=  (([],i ,ind, pos), Nope)	-- we are done
					tk (i:is, j, ind, pos@(line, _))	-- count newlines
								 =((is,j, ind+1, if i == j then (line+1, ind+1) else pos), Res i)

full			:: Eq i => Consumer i e a -> Consumer i e a
full cns		=  do	a <- cns
				rest <- resting
				if null rest then return a
					else abort
{- Keeps all 'i' in the queue that satisfy the given condition.-}
filter			:: (i -> Bool) -> Consumer i e ()
filter cond		=  Consumer $ \ (is, i, ind, pos) -> ((Prelude.filter cond is,i, ind, pos), Res () )

{-| Returns the first parser, if this one has a result or throws an exception|-}
(>:)			:: Eq i => Consumer i e a -> Consumer i e a -> Consumer i e a
(>:) cns backup		=  Consumer $ \ st -> pref (run cns st) backup st
				where 	pref		:: Result i e a -> Consumer i e a -> State i -> Result i e a
					pref (st, Res res) _ _		=  (st, Res res)
					pref (st, Exc e)    _ _		=  (st, Exc e)
					pref (_, Nope)      b st	=  run b st

(>:>)			:: Eq i => [Consumer i e a] -> Consumer i e a
(>:>) []		=  error "For >:>, an empty list is not allowed"
(>:>) (r:rs)		=  foldl (>:) r rs

{- Keeps on consuming until the given consumer gives Nope. All the results are given back into one list.
	The consumer is used at least once.
-}
while			:: Eq i => Consumer i e a -> Consumer i e [a]
while cns		=  do	a <- cns
				_while cns [a]

_while			:: Eq i => Consumer i e a -> [a] -> Consumer i e [a]
_while cns acc		=  (do	a <- cns
				_while cns (a:acc))
			   >: return (reverse acc)

{- Gets the next i tokens -}
for			:: Eq i => Int -> Consumer i e [i]
for i			=  _for i []

_for			:: Eq i => Int -> [i] -> Consumer i e [i]
_for i acc		=  (do	a <- next
				_for (i-1) (a:acc))
			   >: return (reverse acc)
