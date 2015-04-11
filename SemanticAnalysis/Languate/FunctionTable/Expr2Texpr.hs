module Languate.FunctionTable.Expr2Texpr where

import StdDef

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TableOverview


import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import State

{-

Welcome to the lambda core

This is the heart of the typechecker, where expressions get converted into their typed counterpart.

-}

expr2texpr	:: Package -> TableOverview -> FQN -> Expression -> TExpression
expr2texpr p to fqn e
	=  runstate (_e2te e) (Ctx p to fqn) & fst

data Ctx = Ctx	{ package	:: Package
		, tables	:: TableOverview
		, fqn		:: FQN	-- the current location where to search
		}

type SCtx a	= State Ctx a

_e2te	:: Expression -> SCtx TExpression
_e2te e	= todo 




