module Languate.TypeTable.Extended where

{--

This module implements some extended types for the typetable, intended for internal used

--}

import StdDef

import Languate.TypeTable
import Languate.TAST

import Data.Set


type FullSTTEntry	= ([(Name, Set RType)], Maybe RType, (RType, Binding))
type FullSTTKeyEntry	= (RType, FullSTTEntry)
