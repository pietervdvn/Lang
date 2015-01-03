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
		  let lastRow	= [show $ 1 + maximum (keys class2ops), "Other operators", show PrecLeft] in
		  let lastRow'	= [lastRow] in
		  let perClass	= table ["Precedence","Operators","Associativity"] (rows++lastRow') in
		  let rows'	= map (uncurry (op2md modifs)) $ toList op2classes in
		  let perOp	= table ["Operator","Precedence", "Associativity"] rows' in
			title 1 "Precedences overview" ++ explanation ++ parag perClass++parag perOp


class2md	:: Map Name PrecModifier -> Int -> [Name] -> [MarkDown]
class2md mods i ops@(repr:_)
		=  [show i, intercal ", " $ map sOp ops, precOf mods repr]

op2md		:: Map Name PrecModifier -> Name -> Int -> [MarkDown]
op2md mods op i	=  [sOp op, show i, precOf mods op]

precOf	:: Map Name PrecModifier -> Name -> String
precOf mods op
	=  maybe "left (default)" show $ lookup op mods

sOp	:: Name -> String
sOp op	=  "``"++op++"``"

explanation
	= "The higher the operator stands in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated\n\nTo test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation.\n\n"
