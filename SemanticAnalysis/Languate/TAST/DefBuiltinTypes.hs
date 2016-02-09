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
unitTypeCons	= TCall (noConstr unitType) $ Signature unitTypeFQN "Unit" (noConstr unitType)



-------------- ABOUT THE TUPLES ----------------------

-- The representation of a tuple
tupleType	:: RType
tupleType	= uncurry RNormal tupleTypeID

tupleType'	:: CType -> CType -> CType
tupleType' (a,ca) (b,cb)
		= (RApplied (RApplied tupleType a) b, ca ++ cb)

tupleTypeID	= (tupleTypeFQN,"Tuple")
tupleCall	= Call "Tuple"

tupleTypeFQN	= toFQN' "pietervdvn:Data:Collection.Tuple"
tupleTypeConsSign'	:: CType -> CType -> Signature
tupleTypeConsSign' ca@(a, _) cb@(b, _)
		= let  	(tuplT, constr)	= tupleType' ca cb
			tp		= RCurry a (RCurry b tuplT) in
		     Signature tupleTypeFQN "Tuple" (tp, constr)
tupleTypeConsSign
		= tupleTypeConsSign' (noConstr $ RFree $ defaultFreeNames !! 0) (noConstr $ RFree $ defaultFreeNames !! 1)

tupleTypeCons	:: CType -> CType -> TExpression
tupleTypeCons a b
		= let 	sign	= tupleTypeConsSign' a b in
			TCall (signType sign) tupleTypeConsSign


buildTuple	:: [CType] -> [TExpression] -> TExpression
buildTuple [] []= unitTypeCons
buildTuple [t] [e]
		= e
buildTuple ftps@(t@(rt,constr):tps) (e:exps)
		= let 	tail	= buildTuple tps exps	-- Tuple b (Tuble c)
			resultT@(resultRT, resultTConstr)
				= tupleTypes ftps in
			-- (a, (b, c)) = Tuple a (b,c) = TApp (TApp cons a) (b,c)
			TApplication resultT
				(TApplication (RCurry rt $ resultRT, constr ++ resultTConstr)
					(tupleTypeCons t resultT)
					e)
				tail


{-
The type "(a,b)" is represented as ''(RApplied (RApplied Tuple a) b)''
The type "(a,b,c)" is represented as "(a,(b,c))", thus
	''(RApplied (RApplied Tuple a) (RApplied (RApplied Tuple b) c) )''
-}
-- as ((tuple as) rest)
tupledTypes	:: CType -> [CType]
tupledTypes t@(RApplied (RApplied tupleT a) rest, constr)
 | tupleT == tupleType
		= (a,constr):tupledTypes (rest, constr)
 | otherwise	= [t]	-- not a correct application of the tuple type
tupledTypes t
 | t == noConstr unitType
 		= []
 | otherwise	= [t]


tupleTypes	:: [CType] -> CType
tupleTypes []	=  noConstr unitType
tupleTypes [t]	= t
tupleTypes ((t,constr):ts)
		= let (t', constr')	= tupleTypes ts in
		 	(RApplied (RApplied tupleType t) t', constr ++ constr')



-----------------------------------------------------






listType	= uncurry RNormal listTypeID
listTypeID	= (toFQN' "pietervdvn:Data:Collection.List","List")

setType	= uncurry RNormal setTypeID
setTypeID	= (toFQN' "pietervdvn:Data:Collection.Set","Set")

charType	= uncurry RNormal charTypeID
charTypeID	= (charTypeFQN, "Char")
charTypeFQN		= toFQN' "pietervdvn:Data:Data.Char"
charTypeConstr	= TCall (noConstr $ RCurry natType charType) $
		     Signature charTypeFQN "Char" $ noConstr $ RCurry natType charType
charTypeConstr'	:: TExpression -> TExpression
charTypeConstr'	= TApplication (noConstr charType) charTypeConstr



natFQN		= toFQN' "pietervdvn:Data:Data.Nat"

natType		:: RType
natType		= uncurry RNormal natTypeID
natTypeID	= (natFQN, "Nat")

zeroType	= uncurry RNormal zeroTypeID
zeroTypeID	= (natFQN, "Zero")

natType'	:: RType
natType'	= uncurry RNormal natTypeID'
natTypeID'	= (natFQN, "Nat'")

-- Zero Constructor for the natural type
natTypeZero	:: TExpression
natTypeZero	= TCall (noConstr zeroType) $ Signature natFQN "Zero" $ noConstr zeroType

natTypeSucc	:: TExpression
natTypeSucc	= TCall (noConstr $ RCurry natType natType') $
			Signature natFQN "Succ" $ noConstr $ RCurry natType natType

natTypeSucc'	:: TExpression -> TExpression
natTypeSucc'	= TApplication (noConstr natType') natTypeSucc

intType		= uncurry RNormal intTypeID
intTypeID	= (natFQN, "Int")

intType'	= uncurry RNormal intTypeID'
intTypeID'	= (natFQN, "Int'")


floatType	= uncurry RNormal floatTypeID
floatTypeID	= (toFQN' "pietervdvn:Data:Num.Float", "Float")

maybeType	:: RType
maybeType	= uncurry RNormal maybeTypeID

maybeType'	:: CType -> CType
maybeType' (rt, constr)
		= (RApplied (maybeType) rt, constr)

maybeTypeID	= (maybeTypeFQN, "Maybe")
maybeTypeFQN	= toFQN' "pietervdvn:Data:Collection.Maybe"

maybeTypeNothing ct@(rt, constr)
		= TCall (fst $ maybeType' ct, constr) $
			Signature maybeTypeFQN "Nothing" (maybeType' $ noConstr $ RFree $ head defaultFreeNames)
maybeTypeJust ct@(rt, constr)
        = TCall ((RCurry rt (maybeType' ct & fst)), constr) $
            Signature maybeTypeFQN "Just" (maybeType' $ noConstr $ RFree $ head defaultFreeNames)

maybeTypeJust'  :: CType -> TExpression -> TExpression
maybeTypeJust' rt exp
        = TApplication (maybeType' rt)
               (maybeTypeJust rt) exp


stringType	= RApplied listType charType


noConstr	:: RType -> CType
noConstr rt	= (rt, [])
