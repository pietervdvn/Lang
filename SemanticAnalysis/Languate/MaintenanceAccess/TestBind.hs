module Languate.MaintenanceAccess.TestBind where

{--
This module builds all the stuff!
--}

import StdDef
import Exceptions

import Languate.FQN
import Languate.TAST
import Languate.TypeConstraint

import Languate.MaintenanceAccess.TestBuild

import Data.Set as S


stringFQN	= toFQN' "pietervdvn:Data:Data.String"
charFQN		= toFQN' "pietervdvn:Data:Data.Char"
boolFQN		= toFQN' "pietervdvn:Data:Data.Bool"
listFQN		= toFQN' "pietervdvn:Data:Collection.List"
setFQN		= toFQN' "pietervdvn:Data:Collection.Set"
eqFQN		= toFQN' "pietervdvn:Data:Category.Eq"
testingFQN	= toFQN' "pietervdvn:Data:FunctionTesting"

nat		= RNormal natFQN "Nat"
nat'		= RNormal natFQN "Nat'"
zero		= RNormal natFQN "Zero"
string		= RNormal stringFQN "String"
char		= RNormal charFQN "Char"

bool		= RNormal boolFQN "Bool"
list a		= RApplied (RNormal listFQN "List") a
set a		= RApplied (RNormal setFQN "Set") a
eq		= RNormal eqFQN "Eq"
comm a b	= RApplied (RApplied (RNormal testingFQN "Commutative") a) b

a	= RFree "a"
b	= RFree "b"
c	= RFree "c"
d	= RFree "d"

isSpr a b
	= isConstraintMet tt (SubTypeConstr a b) & runExceptionsIO'

(~>)	= RCurry
(&&&) a b
	= RConj [a, b]

cs 	= S.fromList

scs a b	= cs [sc a b]

sc	= SubTypeConstr
