module StateT where

import Control.Monad
import Control.Monad.Trans

newtype StateT s m a = StateT (s -> m (a, s))

runstateT		:: (Monad m) => StateT s m a -> s -> m (a,s)
runstateT (StateT f)	= f

bindST	:: (Monad m) => StateT s m a -> (a -> StateT s m b) -> StateT s m b
bindST ma f
	= StateT $ \ s -> _h ma f s

_h	:: (Monad m) => StateT s m a -> (a -> StateT s m b) -> s -> m (b,s)
_h sma f s	=
	 do	(a, newState)	<- runstateT sma s
		runstateT (f a) newState

instance (Monad m) => Monad (StateT s m) where
	return a	= StateT $ \ s -> return (a,s)
	(>>=)		= bindST

instance MonadTrans (StateT s) where
	lift ma	= StateT $ \ s -> do	a	<- ma
					return (a,s)
instance (Monad m) => Applicative (StateT s m) where
	pure		= return
	(<*>) sf sa 	= do 	f	<- sf
	            		a	<- sa
    			        return $ f a


instance (Monad m) => Functor (StateT s m) where
	fmap f m	= do	a	<- m
				return $ f a

isolate	:: (Monad m) => StateT s m a -> StateT s m a
isolate m	= do	s	<- get
			v	<- m
			put s
			return v

isolate'	::  (Monad m) => StateT s m a -> StateT s m (a,s)
isolate' m	= do	s	<- get
			v	<- m
			s'	<- get
			put s
			return (v,s)

injState	:: Monad m => StateT s m a -> StateT s m a -> (m (a,s) -> m (a,s) -> m (a,s)) -> StateT s m a
injState a b f	=  do	s	<- get
			let mas	= runstateT a s
			let mbs	= runstateT b s
			(res, _)	<- lift $ f mas mbs
			return res


emulateT	:: Monad m => StateT s m a -> StateT s m (m (a,s))
emulateT m	=  do	s	<- get
			return $ runstateT m s

get	:: (Monad m) => StateT s m s
get	=  StateT $ \ s -> return (s,s)

get'	:: (Monad m) => (s -> a) -> StateT s m a
get' f	=  do	s	<- get
		return $ f s

put	:: (Monad m) => s -> StateT s m ()
put s	=  StateT $ \ _ -> return ((),s)



modify	:: (Monad m) => (s -> s) -> StateT s m ()
modify f	= do	cur	<- get
			put $ f cur
