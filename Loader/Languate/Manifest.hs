module Manifest where

{--
This module implements the meta-info package of a manifest
--}

import StdDef

import Languate.FQN

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

import Control.Arrow

type Version	= [Int]
data Manifest = Manifest {name :: Name, synopsis :: String, description :: String,
				version :: Version, language	:: Version,
				authors :: [Name],
				depends :: Map FQN Version,
				prelude	:: Set FQN,
				exposes	:: Set FQN,
				execute	:: Name,
				rest	:: Map Name MetaValue
				}


data MetaValue	= Int Int
		| String String
		| FuncName String
		| ModuleName FQN
		| License License
		| Version Version
		| St   [MetaValue]
		| Lst  [MetaValue]
		| Dict [(MetaValue,MetaValue)]

data License	= GPL
		| Mit
		| None
		| File Name FilePath
	deriving (Show)

{- Some values (e.g. 'author') may automatically become a new key ('authors').
This function says which these are
-}
toMultiple	:: Map String (String, MetaValue -> MetaValue)
toMultiple = M.fromList [ ("author",("authors", asList))
			, ("maintainer",("maintainers", asList))
			]
		where asList	= Lst . (:[])

{- Some keys should have a certain type, given here in the form of a meta value.
All other fields are free in format, and might be used by other tools.
-}
typeOf		:: Map String MetaValue
typeOf	= M.fromList	[ ("version", Version [])
			, ("authors", Lst [String ""])
			, ("maintainers", Lst [String ""])
			, ("dependencies", Dict [(String "", Version [])])
			, ("prelude", St [fqnT])
			, ("license", License None)
			, ("exposes", St [fqnT])
			, ("execute", FuncName "")
			, ("aliases", Dict [(String "", FuncName "")])
		   	  ]
		where fqnT	= ModuleName $ error "pietervdvn is upset"















 -----------
 -- Utils --
 -----------

instance Show MetaValue where
	show	= smv

smv	:: MetaValue	-> String
smv (Int i)	= show i
smv (String s)	= show s
smv (FuncName str)	= str
smv (ModuleName fqn)	= show fqn
smv (License l)	= show l
smv (Version v)	= v |> show & intercalate "."
smv (St vals)	= "{"++ commaSepSmv vals ++"}"
smv (Lst vals)	= "["++ commaSepSmv vals ++ "]"
smv (Dict vals)	= let contents	= vals |> (\(a,b) -> show a ++" --> "++show b)
					& intercalate ", " in
			"{"++ contents ++ "}"
commaSepSmv vals	= vals |> show & intercalate ", "


instance Show Manifest where
	show	= sman

sman m
 = let 	lines m	= FuncName $ flip replicate '-' $ length $ name m
	nl _	= FuncName ""
	s v m	= String $ v m
	f v m	= FuncName $ v m
	v v m	= Version $ v m
	lst v m	= Lst . map String $ v m
	st v m	= St . map ModuleName . S.toList $ v m in
	[ s name
	, lines
	, s synopsis
	, nl
	, s description
	, lines
	, nl
	, lst authors
	, metaValue' "maintainers" $ Lst []
	, v version
	, metaValue' "license" (License None)
	, nl
	, nl
	, v language
	, showDeps . depends
	, st prelude
	, st exposes
	, f execute
	, s (showDict . rest)
	] |> (\f -> f m) |> show & unlines

--}


showDict	:: Map String MetaValue -> String
showDict dict	= dict	|> show
			& M.toList
			|> (\(key, value) -> key ++"\t= "++value)
			& unlines


metaValue	:: String -> Manifest -> Maybe MetaValue
metaValue key 	= M.lookup key . rest

metaValue'	:: String -> MetaValue -> Manifest -> MetaValue
metaValue' key v
		= M.findWithDefault v key . rest

showDeps	:: Map FQN Version -> MetaValue
showDeps dict	= dict & M.toList |> (first ModuleName) |> (second Version) & Dict



-- The last argument is considered the type representative
hasType	:: MetaValue -> MetaValue -> Bool
hasType (Int _)		(Int _)		= True
hasType (String _)	(String _)	= True
hasType (FuncName _)	(FuncName _)	= True
hasType (License _)	(License _)	= True
hasType (Version _)	(Version _)	= True
hasType (ModuleName _)	(ModuleName _)	= True
hasType (St vals)	(St [val])	= all (`hasType` val) vals
hasType (Lst vals)	(Lst [val])	= all (`hasType` val) vals
hasType (Dict vals)	(Dict [val])	= all (`hasType` fst val) (vals |> fst) &&
					  all (`hasType` snd val) (vals |> snd)
hasType _ _	= False
