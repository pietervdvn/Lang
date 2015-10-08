module Languate.Precedence.PrecTable2MU where

import Prelude hiding (lookup)
import StdDef

import Data.Maybe
import Data.Map hiding (map, foldr, null)
import qualified Data.Set as S
import Data.Set (member, Set)
import Data.List (sort)

import Languate.AST
import Languate.MarkUp as Mu
import Languate.Precedence.PrecedenceTableDef



instance Documentable PrecedenceTable where
	toDocument precT
		= (precedenceOverview, [precedencePerLevel precT, precedencePerOperator precT, rulesOverview precT])


precPerLevelTitle	= "Operators per precedence levels"
precPerOpTitle		= "Operators with precedence levels"
precRules		= "Defined rules"

precedencePerLevel	:: PrecedenceTable -> Doc
precedencePerLevel precTable
	= let	rows	= i2op precTable & toList & sort |> class2MU (op2precMod precTable)
		mxm	= maxI precTable
		lastRw1	= [show $ 1 + mxm, "Other operators", show PrecLeft]
		lastRw2	= [show $ 2 + mxm, "Function application", show PrecLeft]
		lastRows= [lastRw1, lastRw2] ||>> Base
		perLvl	= table ["Precedence","Operators","Associativity"] (rows++lastRows)
		in  doc precPerLevelTitle "Gives the different precedence levels with each operator in this precedence level" perLvl


precedencePerOperator	:: PrecedenceTable -> Doc
precedencePerOperator precTable
	= let	rows	= op2i precTable & toList |> op2mu (op2precMod precTable)
		perOp	= table ["Operator","Precedence", "Associativity"] rows in
		doc precPerOpTitle "Gives all the operators with their respective precedence level" perOp

precedenceOverview	:: Doc
precedenceOverview	= doc "Precedence Overview" "Operators with their respective precedence" $
				 titling "Precedence Overview" $ Mu.Seq [explanation, Embed precPerLevelTitle, Embed precPerOpTitle, Embed precRules]


rulesOverview	:: PrecedenceTable -> Doc
rulesOverview precT
		= let	rows	= rules precT |> (\(namesMods, rels , fqn) -> [showR (namesMods, rels), code $ show fqn])	:: [[MarkUp]]	in
			doc precRules "Gives all the known rules about precedence and the module that defines them" $ table ["Rule","Defined in"] rows

showR	:: ([(Name, PrecModifier)], [PrecRelation]) -> MarkUp
showR (namesMods, rels)
	= let	namesMods'	= namesMods |> (\(n, mod) -> n ++ " is "++show mod) |> code
		rels'		= rels |> show |> code in
		(namesMods' ++ rels') |> Parag & Mu.Seq



op2mu	:: Map Name PrecModifier -> (Operator, Int) -> [MarkUp]
op2mu mods (op, i)
	=  [code op, Base $ show i, precOf mods op]

class2MU:: Map Operator PrecModifier -> (Int, Set Operator) -> [MarkUp]
class2MU mods (i, ops')
	=  let ops@(repr:_) = S.toList ops' in
			[Base $ show i, ops |> code & commas', precOf mods repr]

precOf	:: Map Operator PrecModifier -> Name -> MarkUp
precOf mods op
	= Base $ maybe "left (default)" show $ lookup op mods


explanation
	= Mu.Seq [parag "The higher the operator stands in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated\n\nTo test precedence, invoke ````--ppe <expression>```` in the interpreter, which converts expression to prefix notation.", Parag $ Mu.Seq [Base "To change the order, type ", code "precedence of (op) is left/right/prefix/postfix, (op) < (op)"]]
