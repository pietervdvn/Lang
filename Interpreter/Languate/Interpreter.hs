module Languate.Interpreter where

{--

This module implements a simple interpreter for languate.

It takes a map (FQN -> SimpleSymbolTable), where it finds the implementations.


TODO fix this:
It hopes the expressions and functions are type correct...

--}
import Data.Map
import Languate.SymbolTable

type Context	= Map FQN SimpleTable

interpret	:: Expression -> String
