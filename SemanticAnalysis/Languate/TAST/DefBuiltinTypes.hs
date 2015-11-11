module Languate.TAST.DefBuiltinTypes where

{--
This module implements builtin types and builtin expressions, used while typechecking and desugaring
--}

import Languate.TAST.DefType
import Languate.TAST.DefExpr
import Languate.AST

import Languate.FQN

-- The (implicit) supertype for every type
anyType		= uncurry RNormal anyTypeID
anyTypeID	= (toFQN' "pietervdvn:Data:Any", "Any")

boolType	= uncurry RNormal boolTypeID
boolTypeID	= (toFQN' "pietervdvn:Data:Data.Bool", "Bool")

voidType	= uncurry RNormal voidTypeID
voidTypeID	= (voidTypeFQN,"Void")
voidTypeFQN	= toFQN' "pietervdvn:Data:Collection.Void"
voidTypeCons	:: TExpression
voidTypeCons	= TCall ([voidType], []) $ Signature voidTypeFQN "Void" ([voidType], [])

-- The representation of a tuple
tupleType	= uncurry RNormal tupleTypeID
tupleType'	:: String -> String -> RType
tupleType' a b	= RApplied (RApplied tupleType $ RFree a) $ RFree b
tupleTypeID	= (tupleTypeFQN,"Tuple")
tupleCall	= Call "Tuple"
tupleTypeFQN	= toFQN' "pietervdvn:Data:Collection.Tuple"
tupleTypeCons	:: TExpression
tupleTypeCons	= let tp = RCurry (RFree "a")
			  (RCurry (RFree "b")
			  (tupleType' "a" "b")) in
			 TCall ([ tp ],[]) $
		     Signature tupleTypeFQN "Tuple" ([tp], [])

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

natType'	= uncurry RNormal natTypeID'
natTypeID'	= (natFQN, "Nat'")

-- Zero Constructor for the natural type
natTypeZero	:: TExpression
natTypeZero	= TCall ([natType],[]) $ Signature natFQN "Zero" ([natType], [])

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
maybeTypeID	= (toFQN' "pietervdvn:Data:Collection.Maybe", "Maybe")

stringType	= RApplied listType charType



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
 | t == voidType= []
 | otherwise	= [t]


tupleTypes	:: [RType] -> RType
tupleTypes []	=  voidType
tupleTypes [t]	= t
tupleTypes (t:ts)
		= RApplied (RApplied tupleType t) (tupleTypes ts)
