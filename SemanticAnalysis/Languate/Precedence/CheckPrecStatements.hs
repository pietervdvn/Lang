module Languate.Precedence.CheckPrecStatements where

import StdDef
import Exceptions
import Languate.CheckUtils
import Languate.AST
import Languate.FQN

import Data.Tuple
import Data.Maybe
import Data.Set (Set, fromList, member)
import qualified Data.Set as S
import Data.Map (Map, findWithDefault)
import Data.List (nub, sort, intercalate)
import Languate.Precedence.PrecedenceTable
import Languate.Precedence.Utils



-------------------
-- CHECKS BEFORE --
-------------------


{--
- Checks that the operator used is declared in the same module
--}
checkPrecStmsIn	:: FQN -> Module -> Check
checkPrecStmsIn fqn mod
	= do	let stms	= statements' mod
		let available	= stms |> fst >>= declaredOps
		let available'	= fromList available
		let precs	= stms |> swap ||>> declaredPrec |> swap |> unpackMaybeTuple
		let precs'	= catMaybes precs
		mapM_ (\(precAnnot, coor) -> onLocation (fqn,coor) $
			checkPrecStms available' precAnnot)
			 precs'

checkPrecStms	:: Set Name -> PrecedenceAnnot -> Check
checkPrecStms available p@(PrecAnnot {operator = op, relations = rels})
	= do	let msg	= "A precedence declaration must be in the same module it's operator is declared.\n"++
			show p ++" declares the precedence of '"++op++"', but this function is not declared here."
		assert (op `member` available) msg
		checkPrecRels rels

checkPrecRels	:: [PrecRelation] -> Check
checkPrecRels rels
	= do	let eqs	= eqRelations rels |> normalizeTuple
		let showEq (op0, op1)	= op0 ++ " = "++ op1
		assert' (unique eqs) $ "You stated a same precedence relationship multiple times: " ++ intercalate ", " (dubbles eqs |> showEq)
		let lts	= ltRelations rels
		let showLt (op0, op1)	= op0 ++ " < "++ op1
		assert' (unique lts) $ "You stated a lower precedence relationship multiple times: " ++ intercalate ", " (dubbles lts |> showLt)
		inside "Contradictory precedence statement:" $ do
			let checkOne f lt	= assert (f lt `notElem` eqs) $ "You defined both "++ showEq (f lt) ++" and "++ showEq lt
			mapM_ (checkOne id)   lts
			mapM_ (checkOne swap) lts
			let checkOne' lt	= assert (swap lt `notElem` lts) $ "You defined both "++ showLt (swap lt) ++" and "++ showLt lt
			mapM_ checkOne' lts





-- Gets declared function names
declaredOps	:: Statement -> [Name]
declaredOps (FunctionStm f)
	= signs f |> (\(n,_,_) -> n)
declaredOps (ADTDefStm (ADTDef _ _ _ sums))
	= let   maybeFields	= (sums >>= (\(ADTSum _ _ fields) -> fields)) |> fst in
		catMaybes maybeFields
declaredOps (ClassDefStm cd)
	= decls cd |> (\(n,_,_) -> n)
declaredOps _	= []

declaredPrec	:: Statement -> Maybe PrecedenceAnnot
declaredPrec (PrecedenceStm p@(PrecAnnot {}))
	= Just p
declaredPrec _	= Nothing













------------------
-- CHECKS AFTER --
------------------





-- checks that a class is consistent, thus that it contains not both left and right operators
-- checkNoMix	:: PrecedenceTable -> PrecedenceTable
checkNoMix	:: PrecedenceTable -> Exceptions String String ()
checkNoMix table
		= mapM_ (checkClass table) [1..maxI table]


checkClass	:: PrecedenceTable -> Int -> Exceptions String String ()
checkClass (PrecedenceTable maxI _ i2op modifs _) i
		= do	haltIf (i > maxI) $ "Trying to check operator category "++show i++" on consistency, but only "++show maxI++" operator categories exists. This is a bug"
			haltIf (i <= 0) $ "Trying to check operator category "++show i++" on consistency, but categories start numbering from 1 (and not 0). This is a bug"
			let allOps	= S.toList $ findWithDefault S.empty i i2op
			let opsWithMod	= map (\op -> (op, findWithDefault PrecLeft op modifs)) allOps
			let mods	= nub opsWithMod
			let errMods	= merge $ map swap opsWithMod
			let msg		= "Class "++show i++" is not consistent, there is a mixed precedence mode.\n"++
						"The found modes are: "++show mods++" with following operators: \n"++show errMods
			assert (allSame (opsWithMod |> snd)) msg



errorMsg	:: [(Name, Name)] -> String
errorMsg faults	=  "Error: some operators have conflicting precedence relations: \n"
			++ foldr (\ops acc ->  acc ++ errorMsg' ops ) "\n" faults

errorMsg' (o1,o2)	= "\tBoth ("++o1++") = ("++o2++") and ("++o1++") < ("++o2++") are defined.\n"
