module Languate.FunctionTable.TypePattern where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TableOverview
import Languate.FunctionTable
import Languate.BuiltIns
import Languate.TypeTable.Bind.Bind

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import HumanUtils

import Control.Arrow

import Debug.Trace

-- Types the given pattern (or gives an exception)
typePattern	:: (Expression -> Exc [TExpression]) -> (RType -> RType -> Bool) -> FunctionTable -> RType -> Pattern -> Exc (TPattern, Map Name RType)
typePattern _ _ _ tp (Assign nm)
		= return (TAssign nm, M.singleton nm tp)
typePattern typeExpr isSubType ft tp pat@(Multi pats)
		= do	(tpat, dicts) 	<- pats	|> typePattern typeExpr isSubType ft tp & sequence
						|> unzip |> first TMulti
			inside ("While typing the pattern "++show pat) $ do
				dict	<- mergeDicts dicts
				return (tpat, dict)
typePattern _ _ _ _ DontCare
		= return (TDontCare, M.empty)
typePattern typeExpr isSubType ft tp (Deconstruct nm pats)
		= do	-- searches all constructors (functions) we want the "fromNm"
			signs	<- (ft & known & M.lookup ("from"++nm)) ? ("The constructor "++nm++" (desugared to 'from"++ nm ++"') is not in scope")
			-- the type we want: tp -> Maybe (a,b,...)
			let posSigns	= signs |> (deconsType isSubType tp . head . signTypes &&& id)
						|> unpackMaybeTuple & catMaybes
			haltIf (null posSigns) $ "The deconstructor "++show nm++ " is not applicable to "++st True tp++", no applicable function (from"++nm++") found"
			assert (length posSigns == 1) $ "Multiple deconstructions are possible for "++show nm++".\nThe following function are usable: "++
				indent ('\n': posSigns |> snd |> show & unlines)
			let (args, sign)= head posSigns	:: ([RType], Signature)
			assert (length args == length pats) $ show nm ++ " deconstructs into "++plural (length args) "argument" ++", but "++plural (length pats) "pattern" ++ "are given:"++indent ("\n"++nm++" -> "++ args |> show & intercal "\t" ++ "\nPatterns: "++pats |> show & intercal "\t")
			(tpats, scopes)	<- zip args pats |> uncurry (typePattern typeExpr isSubType ft)
							& sequence |> unzip
			scope	<- mergeDicts scopes
			return (TDeconstruct sign tpats, scope)
typePattern typeExpr _ ft tp (Eval expr)
		= do	typedExprs	<- typeExpr expr
			typedExpr	<- case length typedExprs of
						0	-> halt $ "Typing the eval pattern "++show expr++" has no result"
						1	-> return $ head typedExprs
						_	-> do	warn $ "Typing the eval pattern "++show expr++" has multiple results"
								return $ head typedExprs
			return (TEval $ typedExpr, M.empty)	-- TODO check if type *is* a subtype type
typePattern _ _ _ tp pat
		= do	err $ "Non-covered pattern "++show pat
			return (TDontCare, M.empty)

{- given the argument type, is the given function type the one deconstructing it?
Returns the types to which it deconstructs
-}
deconsType	:: (RType -> RType -> Bool) -> RType -> RType -> Maybe [RType]
deconsType isSubtype argType (RCurry deconsType (RApplied maybeT args))
 | argType `isSubtype` deconsType	-- the type we get (arg) should be a subtype of the type that function deconstructs (deconsType)
   && maybeT == maybeType	-- we cant check equality in pattern matches :p
	= Just $ tupledTypes args
deconsType _ _ _
	= Nothing


mergeDicts	:: [Map Name RType] -> Exc (Map Name RType)
mergeDicts dicts = do
	let allkeys	= dicts >>= M.keys
	assert (unique allkeys) $ "Multiple declarations of the variables "++(dubbles allkeys |> show & unwords)
	return $ M.unions dicts
