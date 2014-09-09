module Languate.Interpreter.Utils where

{--

This module implements some interpreter utils, small misch functions which are usefull in the entire interpreter

--}

import StdDef
import Languate.InterpreterDef


mergeBindings	:: [ Binding ] -> Binding
mergeBindings 	=  merge . concatMap unmerge




