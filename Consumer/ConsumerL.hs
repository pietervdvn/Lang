module ConsumerL where

import Consumer
import StdDef

data ConsumerL i e a 	= ConsumerL (State i -> [Result i e a])

runL			:: ConsumerL i e a -> State i -> [Result i e a]
runL (ConsumerL f)	=  f

getStateL		:: [Result i e a] -> [State i]
getStateL		=  map fst

getResL			:: [Result i e a] -> [Outcome e a]
getResL			=  map snd

instance Monad (ConsumerL i e) where
	return a	= ConsumerL $ \ st -> [(st, Res a)]
	x >>= f		= ConsumerL $ \ st -> concatMap (exec f) (runL x st)
				where	exec			:: (a -> ConsumerL i e b) -> Result i e a -> [Result i e b]
					exec _ (st, Exc e)	= [(st, Exc e)]
					exec _ (_, Nope)	= []
					exec fn (st, Res a)	= runL (fn a) st


abortL			:: ConsumerL i e a
abortL			=  ConsumerL $ const []

(>*)			:: Consumer i e a -> ConsumerL i e a
(>*) cons		=  ConsumerL $ \ st -> undet $ run cons st
						where 	undet			:: Result i e a -> [Result i e a]
							undet (_, Nope)	= []
							undet r			= [r]

{-| Gives the first consumer, if this one has (at least) one result |-}
(>:>*) 			:: [ConsumerL i e a] -> ConsumerL i e a
(>:*)			:: ConsumerL i e a -> ConsumerL i e a -> ConsumerL i e a
(>:*) cns backup	=  ConsumerL $ \ st -> select st (runL cns st) backup
				where 	select			:: State i -> [Result i e a] -> ConsumerL i e a -> [Result i e a]
					select st [] bu		=  runL bu st
					select _ res _		=  res
(>:>*) []		=  error "(>:>*): empty list"
(>:>*) (r:rs)		=  foldl (>:*) r rs

(?)			:: ConsumerL i e a -> ConsumerL i e a -> ConsumerL i e a
(?) p1 p2		=  ConsumerL $ \ st -> runL p1 st ++ runL p2 st

(??)			:: [ConsumerL i e a] -> ConsumerL i e a
(??) []			=  error "(??): empty list"
(??) (r:rs)		=  foldl (?) r rs

{-| both parsers are executed and return if both have a result|-}
both			:: ConsumerL i e a -> ConsumerL i e a -> ConsumerL i e a
both p1 p2		=  ConsumerL $ \ st -> conc (runL p1 st) (runL p2 st)
				where 	conc 		:: [Result i e a] -> [Result i e a] -> [Result i e a]
					conc [] _	=  []
					conc _ []	=  []
					conc r1 r2	=  r1++r2

all 			:: [ConsumerL i e a] -> ConsumerL i e a
all []			= error "ConsumerL.all: empty list"
all (r:rs)		= foldl both r rs

embed			:: ConsumerL i e [a] -> ConsumerL i e a
embed cns		=  ConsumerL $ \ st -> concatMap mbed  (runL cns st) -- [(st, Outcome [a]]
				where 	mbed			:: Result i e [a] -> [Result i e a]
					mbed (_, Nope)	= []
					mbed (st, Res rs)	= map (\r -> (st, Res r)) rs
					mbed (st, Exc e)	= [(st, Exc e)]

whileL			:: ConsumerL i e a -> ConsumerL i e [a]
whileL cns		=  do	a <- cns
				_whileL cns [a]

_whileL			:: ConsumerL i e a -> [a] -> ConsumerL i e [a]
_whileL cns acc		=  (do	a <- cns
				_whileL cns (a:acc))
			   >:* return (reverse acc)

forL			:: Eq i => Int -> ConsumerL i e [i]
forL i			=  _forL i []

_forL			:: Eq i => Int -> [i] -> ConsumerL i e [i]
_forL i acc		=  (do	a <- (>*) next
				_forL (i-1) (a:acc))
			   >:* return (reverse acc)

-- longest returns the state where the most tokens are consumed.
-- In case of equal consumption, the first is returned
longest			:: ConsumerL i e a -> Consumer i e a
longest cons		=  Consumer $ \ st -> use st $ runL cons st
				where	use		:: State i -> [Result i e a] -> Result i e a
					use st []	=  (st, Nope)
					use _ res	=  _longest res

_longest		:: [Result i e a] -> Result i e a
_longest []		=  error "No longest result"
_longest [res]		=  res
_longest (((is1, j1, i1, pos1),r1):((is2,j2, i2, pos2),r2):is)	
			=  if i1 >= i2 then _longest $ ((is1, j1, i1, pos1),r1):is
			   	else _longest $ ((is2, j2, i2, pos2),r2):is
