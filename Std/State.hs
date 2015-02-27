module State where
import Control.Monad

-- The state monad. My ghci couldn't find it, and I was too lazy to properly install it!


data State s r 	= State (s -> (r,s))

instance Monad (State s) where
	return r		= State (\s -> (r,s))
	(>>=) (sta) (fab)	= State (help sta fab)
		where	help		:: State s a -> (a -> State s r) -> s -> (r,s)
			help sa fastb s	=  let	(a,s1)	= runstate sa s
					   in	runstate (fastb a) s1

instance Functor (State s) where
	fmap	= liftM

get	:: State s s -- I'm not a nazi. All correlation with the "SchutzStaffel" is incidental
get 	=  State (\s -> (s,s))

get'	:: (s -> a) -> State s a	-- this is a coincidence too
get' f	=  do	s	<- get
		return $ f s


put	:: s -> State s ()
put s	=  State (const ((),s))

modify	:: (s -> s) -> State s ()
modify f= do	s	<- get
		put $ f s
		return ()

embed	:: (s -> a) -> State s a
embed f	=  do	s	<- get
		return $ f s

runstate		:: State s r -> s -> (r,s)
runstate (State f)	=  f

-- executes the given state change, until the state is stable
stabilize	:: (Eq s) => State s a -> State s [a]
stabilize act	=  do	s	<- get
			a	<- act
			s'	<- get
			if s == s' then	return [a]
				else do	as	<- stabilize act
					return (a:as)

stabilize_	:: (Eq s) => State s a -> State s ()
stabilize_ act	=  do	s	<- get
			act
			s'	<- get
			when (s /= s') $ stabilize_ act
