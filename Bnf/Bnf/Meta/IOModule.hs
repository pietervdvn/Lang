module Bnf.Meta.IOModule where

import Bnf.BNF
import StdDef
import Bnf.FQN
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map hiding (map, filter)

{--

This module implements the data structure that represents a bnf-module (with more meta), such as imported, private, start rule, etc... 
Not all rules are known in here, but once all files have been parsed, these 'IOModules' are slimmed down to contain only the strictly needed data to parse.
That is the task of the code in Loader.
--}

type Metadata	= Either Int String
type IOMeta	= (Name, Either Metadata [Metadata], Position)
data IOModule	= IOM FQN [IOMeta] [IOImport] [IORule]
	deriving (Show)

type IsInitial	= Bool
data IORule	= IORule Name Expression IsPublic IsInitial Position	-- no is token, as that is done in the converter step (by calling token and converting each Rgx to a WsRgx)
	deriving (Show)


type IsPublic	= Bool
type IsVisible	= Bool
data IOImportMode	= WhiteList | BlackList	-- importmode, corresponding with showing / hiding
	deriving (Show, Eq)
data IOImport	= IOImport FQN IsPublic IOImportMode [(Name, IsVisible)] Position
	deriving (Show)

getImports	:: IOModule -> [IOImport]
getImports (IOM _ _ imps _)	= imps

getFQN		:: IOImport -> FQN
getFQN (IOImport fqn _ _ _ _)
		= fqn

getFqn		:: IOModule -> FQN
getFqn (IOM fqn _ _ _)
		= fqn

getRules	:: IOModule -> [IORule]
getRules (IOM _ _ _ rules)
		= rules

getRuleName	:: IORule -> Name
getRuleName (IORule name _ _ _ _)
		= name

-- converts the IOrules into name --> Expression maps. 
localRules	:: FQN -> [Name] -> IOModule -> Map Name Expression
localRules fqn imps iom	
 		=  let known = S.fromList $ (map (\(_, nm, _) -> nm) $ locallyDefined iom) ++ imps in
			fromList $ map (\(IORule name e _ _ _) -> (name, e)) $ getRules iom

localExports	:: IOModule -> Set (FQN, Name)
localExports	=  S.fromList . map (\(fqn, n, _) -> (fqn, n)) . filter (\(_,_,public) -> public ) . locallyDefined

-- gets all locally defined rules
locallyDefined	:: IOModule -> [(FQN, Name, IsPublic)]
locallyDefined (IOM fqn _ _ rules)
		= map ((\(n, p) -> (fqn, n, p))  . ruleFQN) rules


-- all rules locally defined, with
ruleFQN	:: IORule -> (Name, IsPublic)
ruleFQN (IORule name _ public _ _)
		= (name, public)


