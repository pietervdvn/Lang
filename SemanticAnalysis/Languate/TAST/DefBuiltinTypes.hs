module Languate.TAST.DefBuiltinTypes where

{--
This module implements builtin types and builtin expressions, used while typechecking and desugaring
--}

import StdDef
import Languate.TAST.DefType
import Languate.TAST.DefExpr
import Languate.TAST.Defaults
import Languate.TAST.ExprUtils
import Languate.AST

import Languate.FQN

import Control.Arrow

-- The (implicit) supertype for every type
anyType		= uncurry RNormal anyTypeID
anyTypeID	= (toFQN' "pietervdvn:Data:Any", "Any")

boolType	= uncurry RNormal boolTypeID
boolTypeID	= (toFQN' "pietervdvn:Data:Data.Bool", "Bool")

unitType	= uncurry RNormal unitTypeID
unitTypeID	= (unitTypeFQN,"Unit")
unitTypeFQN	= toFQN' "pietervdvn:Data:Collection.Unit"
unitTypeCons	:: TExpression
unitTypeCons	= TCall ([unitType], []) $ Signature unitTypeFQN "Unit" ([unitType], [])



-------------- ABOUT THE TUPLES ----------------------

-- The representation of a tuple
tupleType	= uncurry RNormal tupleTypeID

tupleType'	:: RType -> RType -> RType
tupleType' a b	= RApplied (RApplied tupleType a) b

tupleTypeID	= (tupleTypeFQN,"Tuple")
tupleCall	= Call "Tuple"

tupleTypeFQN	= toFQN' "pietervdvn:Data:Collection.Tuple"
tupleTypeConsSign'	:: RType -> RType -> Signature
tupleTypeConsSign' a b
		= let 	tp = RCurry a (RCurry b (tupleType' a b)) in
		     Signature tupleTypeFQN "Tuple" ([tp], [])
tupleTypeConsSign
		= tupleTypeConsSign' (RFree $ defaultFreeNames !! 0) (RFree $ defaultFreeNames !! 1)

tupleTypeCons	:: RType -> RType -> TExpression
tupleTypeCons a b
		= let 	sign	= tupleTypeConsSign' a b in
			TCall (signTypes sign) tupleTypeConsSign


buildTuple	:: [RType] -> [TExpression] -> TExpression
buildTuple [] []= unitTypeCons
buildTuple [t] [e]
		= e
buildTuple ftps@(t:tps) (e:exps)
		= let 	tail	= buildTuple tps exps	-- Tuple b (Tuble c)
			resultT	= tupleTypes ftps in
			-- (a, (b, c)) = Tuple a (b,c) = TApp (TApp cons a) (b,c)
			TApplication ([resultT], [])
				(TApplication ([RCurry t $ resultT], [])
					(tupleTypeCons t resultT)
					e)
				tail


{-
The type "(a,b)" is represented as ''(RApplied (RApplied Tuple a) b)''
The type "(a,b,c)" is represented as "(a,(b,c))", thus
	''(RApplied (RApplied Tuple a) (RApplied (RApplied Tuple b) c) )''
-}
-- as ((tuple as) rest)
tupledTypes	:: RType -> [RType]
tupledTypes t@(RApplied (RApplied tupleT a) rest)
 | tupleT == tupleType
		= a:tupledTypes rest
 | otherwise	= [t]	-- not a correct application of the tuple type
tupledTypes t
 | t == unitType= []
 | otherwise	= [t]


tupleTypes	:: [RType] -> RType
tupleTypes []	=  unitType
tupleTypes [t]	= t
tupleTypes (t:ts)
		= RApplied (RApplied tupleType t) (tupleTypes ts)



-----------------------------------------------------






listType	= uncurry RNormal listTypeID
listTypeID	= (toFQN' "pietervdvn:Data:Collection.List","List")

setType	= uncurry RNormal setTypeID
setTypeID	= (toFQN' "pietervdvn:Data:Collection.Set","Set")

charType	= uncurry RNormal charTypeID
charTypeID	= (charTypeFQN, "Char")
charTypeFQN		= toFQN' "pietervdvn:Data:Data.Char"
charTypeConstr	= TCall ([ RCurry natType charType],[]) $
		     Signature charTypeFQN "Char" ([RCurry natType charType], [])
charTypeConstr'	= TApplication ([charType], []) charTypeConstr



natFQN		= toFQN' "pietervdvn:Data:Data.Nat"

natType		= uncurry RNormal natTypeID
natTypeID	= (natFQN, "Nat")

zeroType	= uncurry RNormal zeroTypeID
zeroTypeID	= (natFQN, "Zero")

natType'	= uncurry RNormal natTypeID'
natTypeID'	= (natFQN, "Nat'")

-- Zero Constructor for the natural type
natTypeZero	:: TExpression
natTypeZero	= TCall ([zeroType],[]) $ Signature natFQN "Zero" ([zeroType], [])

natTypeSucc	:: TExpression
natTypeSucc	= TCall ([ RCurry natType natType'],[]) $
			Signature natFQN "Succ" ([RCurry natType natType], [])

natTypeSucc'	:: TExpression -> TExpression
natTypeSucc'	= TApplication ([natType'], []) natTypeSucc

intType		= uncurry RNormal intTypeID
intTypeID	= (natFQN, "Int")

intType'	= uncurry RNormal intTypeID'
intTypeID'	= (natFQN, "Int'")


floatType	= uncurry RNormal floatTypeID
floatTypeID	= (toFQN' "pietervdvn:Data:Num.Float", "Float")

maybeType	= uncurry RNormal maybeTypeID
maybeType' rt   = ([RApplied maybeType rt], [])
maybeTypeID	= (maybeTypeFQN, "Maybe")
maybeTypeFQN	= toFQN' "pietervdvn:Data:Collection.Maybe"

maybeTypeNothing rt
		= TCall (maybeType' rt) $
			Signature maybeTypeFQN "Nothing" (maybeType' $ RFree $ head defaultFreeNames)
maybeTypeJust rt
        = TCall (maybeType' rt & first (|> RCurry rt)) $
            Signature maybeTypeFQN "Just" (maybeType' $ RFree $ head defaultFreeNames)

maybeTypeJust'  :: RType -> TExpression -> TExpression
maybeTypeJust' rt exp
        = TApplication (maybeType' rt)
               (maybeTypeJust rt) exp


stringType	= RApplied listType charType
