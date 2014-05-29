module Bnf.FQN where

{--

This module implements general representation of a Fully Qualified Name

--}

import StdDef
import Data.List

type Author 	= Name
type PName	= Name
type MName	= Name	-- Module name
type Version	= [Int]


-- e.g. FQN ["bnf","rule"] "Content" represents bnf/rule/Content. Note that Content can import everything in Rule* without having to give it's qualified paths; path searching is from inside to the rest
data FQN	= FQN [PName] MName
	deriving (Eq, Ord)

instance Show FQN where
	show (FQN _ mn) 	= mn
