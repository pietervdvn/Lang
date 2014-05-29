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

put	:: s -> State s ()
put s	=  State (const ((),s))

modify	:: (s -> s) -> State s ()
modify f= do	s	<- get
		put $ f s
		return ()

runstate		:: State s r -> s -> (r,s)
runstate (State f)	=  f
