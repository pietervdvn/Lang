module Languate.Manifest.Manifest where

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
import Data.Maybe

import Control.Arrow

type Version	= [Int]
data Manifest = Manifest {name :: Name, synopsis :: String, description :: String,
				version :: Version,
				language:: Version,
				authors :: [Name],
				depends :: Map FQPN Version,
				prelude	:: Set [Name],
				exposes	:: Set [Name],
				execute	:: Maybe Name,
				rest	:: Map Name MetaValue
				}


data MetaValue	= Int Int
		| String String
		| FuncName String
		| GlobId String
		| ModuleName [Name]
		| License License
		| Version Version
		| St   [MetaValue]
		| Lst  [MetaValue]
		| Dict [(MetaValue,MetaValue)]

data KnownLicense
		= None
		| MIT
		| GPL
	deriving (Show, Enum, Bounded)

data License	= Known KnownLicense
		| File Name FilePath

{- Some values (e.g. 'author') may automatically become a new key ('authors').
This function says which these are
-}
preProcess	:: Map String (String, MetaValue -> MetaValue)
preProcess = M.fromList [ ("author",("authors", asList))
			, ("maintainer",("maintainers", asList))
			, ("depends",("dependencies", id))
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
			, ("license", License $ Known None)
			, ("exposes", St [fqnT])
			, ("execute", FuncName "")
			, ("aliases", Dict [(String "", FuncName "")])
		   	  ]
		where fqnT	= ModuleName $ error "pietervdvn is upset"















 -----------
 -- Utils --
 -----------

instance Show License where
	show (Known kl)	= show kl
	show (File n fp)= n++" --> "++fp

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
smv (GlobId s)	= s
commaSepSmv vals	= vals |> show & intercalate ", "



showMetaType		:: MetaValue	-> String
showMetaType (Int i)	= "Int"
showMetaType (String s)	= "String"
showMetaType (FuncName str)	= "Function"
showMetaType (ModuleName fqn)	= "ModuleName"
showMetaType (License l)	= "License"
showMetaType (Version v)	= "Version"
showMetaType (St (v:_))		= "{"++ showMetaType v ++"}"
showMetaType (Lst (v:_))	= "["++ showMetaType v ++ "]"
showMetaType (Dict ((k,v):_))
		=  "{"++ showMetaType k ++ " --> "++showMetaType v ++ "}"
showMetaType (GlobId s)	= "Identifier"


instance Show Manifest where
	show	= sman

sman m
 = let 	lines m	= FuncName $ flip replicate '-' $ length $ name m
	nl _	= FuncName ""
	s v m	= String $ v m
	f v m	= FuncName $ v m
	v v m	= Version $ v m
	lst v m	= Lst . map String $ v m
	st v m	= St . map (String . intercalate "." ) . S.toList $ v m
	vals	= replicate 7 "" ++ ["authors", "maintainers","version","license","","","language","dependencies","prelude","exposes","execute","","",""]
	vals'	= vals |> (\str -> if null str then "" else str++ (if length str < 8 then "\t" else "") ++ "\t= ")
	seen	= filter (/="") vals ++ ["name","synopsis","description"]	in
	[ f name
	, lines
	, f synopsis
	, nl
	, f description
	, lines
	, nl
	, lst authors
	, metaValue' "maintainers" $ Lst []
	, v version
	, metaValue' "license" (License $ Known None)
	, nl
	, nl
	, v language
	, showDeps . depends
	, st prelude
	, st exposes
	, f $ fromMaybe "" . execute
	, nl
	, nl
	, f (showDict . deleteList seen . rest)
	] |> (\f -> f m) |> show & zipWith (++) vals' & unlines

--}

deleteList	:: (Ord k) => [k] -> Map k v -> Map k v
deleteList ks dict
		=  foldr (\k d -> M.delete k d) dict ks


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

showDeps	:: Map FQPN Version -> MetaValue
showDeps dict	= dict & M.toList |> (first (FuncName . show)) |> (second Version) & Dict



-- The last argument is considered the type representative
hasType	:: MetaValue -> MetaValue -> Bool
hasType (Int _)		(Int _)		= True
hasType (String _)	(String _)	= True
hasType (FuncName _)	(FuncName _)	= True
hasType (License _)	(License _)	= True
hasType (Version _)	(Version _)	= True
hasType (ModuleName _)	(ModuleName _)	= True
hasType (St vals)	(St (val:_))	= all (`hasType` val) vals
hasType (Lst vals)	(Lst (val:_))	= all (`hasType` val) vals
hasType (Dict vals)	(Dict ((k,v):_))= all (`hasType` k) (vals |> fst) &&
					  all (`hasType` v) (vals |> snd)
hasType _ _	= False
