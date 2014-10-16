module Languate.MaintenanceAccess.TestRep where

{--

This module implements testfunctions for testing ReadEvalPrint

--}

import System.IO.Unsafe
import qualified  Bnf

import Languate.ReadEvalPrint
import Languate.FQN
import Languate.TypedLoader
import Languate.Interpreter.Application (multApply)

import Data.Maybe

world	= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate.bnf"

fqpn		= fromJust $ toFQPN "pietervdvn:Data"
prelude		= fromJust $ toFqn' fqpn [] "Prelude"
bool		= fromJust $ toFqn' fqpn ["Data"] "Bool"


tpack	= unsafePerformIO $ typedLoad prelude "../workspace/Data/src/"


t	= parseEval world tpack prelude
