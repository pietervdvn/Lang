module Languate.TAST.KindUtils where

{- Some usefull functions to manipulate kinds -}

import StdDef
import HumanUtils
import Languate.TAST.DefType


instance Show Kind where
	show Kind	= "*"
	show k		= _sk k

_sk	:: Kind -> String
_sk Kind = "*"
_sk k	= k & kindArgs & (++[Kind]) |> show & intercal " ~> " & pars


isNormalKind		:: Kind -> Bool
isNormalKind Kind	= True
isNormalKind _		= False

numberOfKindArgs	:: Kind -> Int
numberOfKindArgs	= length . kindArgs


kindArgs	:: Kind -> [Kind]
kindArgs Kind	= []
kindArgs (KindCurry h t)
		= h:kindArgs t


buildKind	:: [Kind] -> Kind
buildKind	= foldr KindCurry Kind
