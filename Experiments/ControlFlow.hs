

import Prelude hiding ((.),(||))
import Control.Applicative (Applicative, (<*>))

(.)	:: a -> (a -> b) -> b
(.) a f	= f a

(||)	:: Functor f => f a -> (a -> b) -> f b
(||) func f
	= fmap f func

(|>)	:: Applicative f => f a -> f (a -> b) -> f b
(|>) app func
	= func <*> app

(|>>)	:: Monad m => m a -> (a -> m b) -> m b
(|>>)	= (>>=)

append	:: String -> String -> String
append str obj
	= obj ++ str
