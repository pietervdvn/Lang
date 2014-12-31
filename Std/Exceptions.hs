module Exceptions where

{--
This module implements a exception monad and tools
--}

import Control.Monad hiding (fail)


runExceptions	:: Exceptions w e a -> ([w], [e], Either e a)
runExceptions (Halt ws es e)		= (ws, es, Left e)
runExceptions (Exceptions ws es a)	= (ws, es, Right a)

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
assert c	= when c . err

assert'	:: Bool -> w -> Exceptions w e ()
assert' c	= when c . warn

halt	:: e -> Exceptions w e ()
halt e	=  Halt [] [] e

haltIf	:: Bool -> e -> Exceptions w e ()
haltIf c= when c . halt
