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

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import HumanUtils

import Control.Arrow

import Debug.Trace

-- Types the given pattern (or gives an exception)
typePattern	:: FunctionTable -> RType -> Pattern -> Exc (TPattern, Map Name RType)
typePattern _ tp (Assign nm)
		= return (TAssign nm, M.singleton nm tp)
typePattern ft tp pat@(Multi pats)
		= do	(tpat, dicts) 	<- pats	|> typePattern ft tp & sequence
						|> unzip |> first TMulti
			inside ("While typing the pattern "++show pat) $ do
			dict	<- mergeDicts dicts
			return (tpat, dict)
typePattern _ _ DontCare
		= return (TDontCare, M.empty)
typePattern ft tp (Deconstruct nm pats)
		= do	-- searches all constructors (functions) we want the "fromNm"
			signs	<- (ft & known & M.lookup ("from"++nm)) ? ("The constructor "++nm++" (desugared to 'from"++ nm ++"') is not in scope")
			-- the type we want: tp -> Maybe (a,b,...)
			let posSigns	= signs |> (deconsType tp . head . signTypes &&& id)
						|> unpackMaybeTuple & catMaybes
			haltIf (null posSigns) $ "The deconstructor "++show ("from"++nm)++ " is not applicable to "++st True tp
			assert (length posSigns == 1) $ "Multiple deconstructions are possible for "++show nm++".\nThe following function are usable: "++
				indent ('\n': posSigns |> snd |> show & unlines)
			let (args, sign)= head posSigns	:: ([RType], Signature)
			assert (length args == length pats) $ show nm ++ " deconstructs into "++plural (length args) "argument" ++", but "++plural (length pats) "pattern" ++ "are given:"++indent ("\n"++nm++" -> "++ args |> show & intercal "\t" ++ "\nPatterns: "++pats |> show & intercal "\t")
			(tpats, scopes)	<- zip args pats |> uncurry (typePattern ft)
							& sequence |> unzip
			scope	<- mergeDicts scopes
			return (TDeconstruct sign tpats, scope)
typePattern ft tp (Eval (Nat i))
		= do	-- TODO check if type *is* a numerical type
			return (TEval $ TNat i, M.empty)
typePattern _ tp pat
		= do	err $ "Non-coverable pattern "++show pat
			return (TDontCare, M.empty)

{- given the argument type, is the given function type the one deconstructing it?
Returns the types to which it deconstructs
-}
deconsType	:: RType -> RType -> Maybe [RType]
deconsType argType (RCurry deconsType (RApplied maybeT args))
 | deconsType == argType 	-- the type we get (arg) should be the type the function deconstructs (deconsType)
   && maybeT == maybeType	-- we cant check equality in pattern matches :p
	= Just $ tupledTypes args
deconsType _ _	= Nothing


mergeDicts	:: [Map Name RType] -> Exc (Map Name RType)
mergeDicts dicts = do
	let allkeys	= dicts >>= M.keys
	assert (unique allkeys) $ "Multiple declarations of the variables "++(dubbles allkeys |> show & unwords)
	return $ M.unions dicts
