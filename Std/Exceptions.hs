module Exceptions where

{--
This module implements a exception monad and tools
--}

import Control.Monad hiding (fail)
import Debug.Trace
import StdDef

runExceptions	:: Exceptions w e a -> ([w], [e], Either e a)
runExceptions (Halt ws es e)		= (ws, es, Left e)
runExceptions (Exceptions ws es a)	= (ws, es, Right a)


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
			when (lWs > 0) $ putStrLn $ concatMap (indent' "\nWarning: ") ws
			when (lEs > 0) $ putStrLn $ concatMap (indent' "\nError: "  ) es
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

data Exceptions w e a	= Exceptions [w] [e] a	| Halt [w] [e] e

instance Monad (Exceptions w e) where
	return a	= Exceptions [] [] a
	(>>=) (Exceptions ws es a) fa2mb
			= let excs	= fa2mb a in
				case excs of
					(Exceptions ws' es' b) 	-> Exceptions (ws ++ ws') (es ++ es') b
					(Halt ws' es' e)	-> Halt (ws ++ ws') (es ++ es') e
	(>>=) (Halt ws es e) _
			= Halt ws es e



{- Stack executes another 'procedure' and converts its result into the current exception.
Usefull for adding a stacktrace:
    do	a <- stack ("In file "++filename++": ") stuffWithFile
-}
stack	:: (w -> w', e -> e') -> Exceptions w e a -> Exceptions w' e' a
stack (fw, fe) (Exceptions ws es a)
	= Exceptions (map fw ws) (map fe es) a
stack (fw, fe) (Halt ws es e)
	= Halt (map fw ws) (map fe es) $ fe e
-- same as stac, but both functions are the same
stack'	:: (we -> we') -> Exceptions we we a -> Exceptions we' we' a
stack' f	= stack (f,f)

warn	:: w -> Exceptions w e ()
warn w	=  Exceptions [w] [] ()

err	:: e -> Exceptions w e ()
err e	= Exceptions [] [e] ()

assert	:: Bool -> e -> Exceptions w e ()
assert c	= unless c . err
errIf c	= assert (not c)

assert'	:: Bool -> w -> Exceptions w e ()
assert' c	= unless c . warn
warnIf c	= assert' (not c)

halt	:: e -> Exceptions w e ()
halt e	=  Halt [] [] e

haltIf	:: Bool -> e -> Exceptions w e ()
haltIf c= when c . halt
