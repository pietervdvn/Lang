module Languate.MaintenanceAccess.TestBind where

{--
This module builds all the stuff!
--}

import StdDef

import Languate.FQN
import Languate.TAST
import Languate.TypeConstraint

import Languate.MaintenanceAccess.TestBuild


stringFQN	= toFQN' "pietervdvn:Data:Data.String"
charFQN		= toFQN' "pietervdvn:Data:Data.Char"
boolFQN		= toFQN' "pietervdvn:Data:Data.Bool"

nat		= RNormal natFQN "Nat"
nat'		= RNormal natFQN "Nat'"
zero		= RNormal natFQN "Zero"

bool		= RNormal boolFQN "Bool"


a	= RFree "a"
b	= RFree "b"
c	= RFree "c"
d	= RFree "d"



(~>)	= RCurry
(&&&) a b
	= RConj [a, b]
