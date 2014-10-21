module Languate.Precedence.PrecTable2MD where

{--

This module implements the conversion from a precedence table into a markdown,. This document contains an overview for the precedence classes.

--}
import StdDef
import MarkDown
import Languate.AST
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.Maybe

precTable2md	:: Map Name Int -> Map Int [Name] -> Map Name PrecModifier -> MarkDown
precTable2md op2classes class2ops modifs
		= let rows	= map (uncurry (class2md modifs)) $ toList class2ops in	-- assumes toList returns sorted
		  let perClass	= table ["Precedence","Operators","Associativity"] rows in
		  let rows'	= map (uncurry (op2md modifs)) $ toList op2classes in
		  let perOp	= table ["Operator","Precedence", "Associativity"] rows' in
			"# Precedences overview\n\n" ++ perClass++"\n\n"++perOp++"\n"


class2md	:: Map Name PrecModifier -> Int -> [Name] -> [MarkDown]
class2md mods i ops@(repr:_)
		=  [show i, intercalate ", " $ map sOp ops, show $ fromJust $ lookup repr mods]

op2md		:: Map Name PrecModifier -> Name -> Int -> [MarkDown]
op2md mods op i	=  [sOp op, show i, show $ fromJust $ lookup op mods]

sOp	:: Name -> String
sOp op	=  "``"++op++"``"
