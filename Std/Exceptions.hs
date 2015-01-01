module Exceptions where

{--
This module implements a exception monad and tools
--}

import Control.Monad hiding (fail)
import Debug.Trace
import StdDef

runExceptions	:: Exceptions w e a -> ([w], [e], Either e a)
runExceptions (Exceptions ws es ea)	= (ws, es, ea)


runExceptionsIO	:: (Show w, Show e) => Exceptions w e a -> IO a
runExceptionsIO exc
		= do	let (ws, es, eOrA)	= runExceptions exc
			_runExceptionsIO' (map show ws, map show es, case eOrA of
					Left e	-> Left $ show e
					Right a	-> Right a)


runExceptionsIO'	:: Exceptions String String a -> IO a
runExceptionsIO' exc
		= do	_runExceptionsIO' $ runExceptions exc

_runExceptionsIO'	:: ([String], [String], Either String a) -> IO a
_runExceptionsIO' (ws, es, eOrA)
		= do	let lEs	= length es
			let lWs	= length ws
			when (lWs > 0) $ putStrLn $ concatMap (indent' "\n\nWarning: ") ws
			when (lEs > 0) $ putStrLn $ concatMap (indent' "\n\nError: "  ) es
			let multi ls	= if (length ls == 0 || length ls >1) then "s" else ""
			let cnt ls word	= show (length ls) ++" "++ word ++ multi ls
			when (lWs + lEs > 0) $ putStrLn $ cnt ws "warning"++", "++cnt es "error"++"."
			case eOrA of
				Left e	-> error  e
				Right a	-> return a


-- Writes (with trace) to terminal. DO NOT USE IN PRODUCTION
testException'	:: (w -> String, e -> String) -> Exceptions w e a -> a
testException' (sw, se) exc
		= let 	(ws, es, eOrA)	= runExceptions exc
		  	blob	= concatMap (indent' "\nWarning: " . sw ) ws ++ concatMap (indent' "\nError: " . se ) es in
			trace blob $ case eOrA of
					Left e	-> error $ se e
					Right a	-> a

{- The Exceptions monad is a kind of writer, to which exceptions and warnings can be written. Even when a exception is encountered, the monad will continue, as to gather as much exceptions as possible. Only the fail will stop execution!-}

data Exceptions w e a	= Exceptions [w] [e] (Either e a)
type Exceptions' e a	= Exceptions e e a

instance Monad (Exceptions w e) where
	return a	= Exceptions [] [] $ Right a
	(>>=) (Exceptions ws es (Right a)) fa2mb
			= let Exceptions ws' es' eOrB	= fa2mb a in
				Exceptions (ws ++ ws') (es ++ es') eOrB
	(>>=) (Exceptions ws es (Left e)) _
			= Exceptions ws es (Left e)


instance Functor (Exceptions w e) where
	fmap f ma 	= do	a <- ma
				return $ f a


{- Stack executes another 'procedure' and converts its result into the current exception.
Usefull for adding a stacktrace:
    do	a <- stack ("In file "++filename++": ") stuffWithFile
-}
stack	:: (w -> w', e -> e') -> Exceptions w e a -> Exceptions w' e' a
stack (fw, fe) (Exceptions ws es eOrA)
	= Exceptions (map fw ws) (map fe es) $ lmap fe eOrA

-- same as stac, but both functions are the same
stack'	:: (we -> we') -> Exceptions we we a -> Exceptions we' we' a
stack' f	= stack (f,f)

warn	:: w -> Exceptions w e ()
warn w	=  Exceptions [w] [] $ Right ()

err	:: e -> Exceptions w e ()
err e	= Exceptions [] [e] $ Right ()

assert	:: Bool -> e -> Exceptions w e ()
assert c	= unless c . err
errIf c	= assert (not c)

assert'	:: Bool -> w -> Exceptions w e ()
assert' c	= unless c . warn
warnIf c	= assert' (not c)

halt	:: e -> Exceptions w e a
halt e	=  Exceptions [] [] (Left e)

haltIf	:: Bool -> e -> Exceptions w e ()
haltIf c e
	= when c $ halt e

pass	:: Exceptions w e ()
pass	= return ()


lmap	:: (a -> c) -> Either a b -> Either c b
lmap f (Left a)	= Left $ f a
lmap _ (Right b)	= Right b


(?)	:: Maybe a -> e -> Exceptions w e a
(?) Nothing e	= halt e
(?) (Just a) _	= return a
