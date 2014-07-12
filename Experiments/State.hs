module State where

{--

This module implements a state monad

--}


type State s a	= s -> (s, a)

runState	:: s -> State s a -> (s, a)
runState start st
		= st start

r	:: a -> State a a
r a	=  const (a,a)

bind	:: State s a -> (a -> State s b) -> State s b
bind ma ma2b s0
	= 	let (s1,a) = ma s0 in
		ma2b a s1

