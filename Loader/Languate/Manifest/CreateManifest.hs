module Languate.Manifest.CreateManifest where

{--
This modules creates a manifest from key-value pairs
--}

import StdDef
import Exceptions
import Languate.Manifest.Manifest
import Languate.FQN

import Data.Char
import Data.List

import Data.Map (Map, findWithDefault, fromList)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

import Control.Monad
import Control.Arrow

import Debug.Trace

type Exc a	= Exceptions' String a


createFromDict	:: Name -> String -> String -> [(String, MetaValue)] -> Exc Manifest
createFromDict name synopsis description pairs
	= stack' ((++) "While building the manifest\n") $
	  do	pairs'	<- pairs |> preProc & preProcs'
		(typesOk, dict)	<- mapM typeCheck pairs' |> unzip |> first and
		haltIf (not typesOk) $ "Not all fields have the correct type"
		let description'	= description & stripnl & reverse & stripnl & reverse
		let fetch str	= lookup str dict ? ("The field '"++str++"' was not found")
		let get def str	= return $ fromMaybe def $ lookup str dict
		let unp (Lst nms)	= nms |> (\(String s) -> s)
		let unpv (Version v)	= v
		version		<- fetch "version" |> unpv
		language	<- fetch "language" |> unpv
		authors		<- fetch "authors" |> unp
		depends'	<- fetch "dependencies"
		depends		<- fetchDeps depends'
		prelude		<- get (St []) "prelude" |> fetchModuleSet
		exposes		<- get (St []) "exposes" |> fetchModuleSet
		let execute	=  lookup "execute" dict |> (\(FuncName f) -> f)
		let rest	= fromList dict
		return $ Manifest name synopsis description' version language authors
					depends prelude exposes execute rest


fetchModuleSet	:: MetaValue -> Set [Name]
fetchModuleSet (St modNms)
	= modNms |> (\(ModuleName nms) -> nms) & S.fromList

fetchDeps	:: MetaValue -> Exc (Map FQPN Version)
fetchDeps (Dict nmsV)
	= do	let (nms, versions)	= unzip nmsV	-- ([MetaValue String], [MetaValue Version])
		let versions'	= versions	|> (\(Version v) -> v)
		let nms'	= nms 		|> (\(String s) -> s)
		let fqpns	= map toFQPN nms'
		let msg str	= "'"++ str ++ "'is not a valid package name"
		let check (str, mFqpn)	= when (isNothing mFqpn) $ err $ msg str
		mapM_ check $ zip nms' fqpns
		zip fqpns versions' |> unpackMaybeTuple & catMaybes & fromList & return


-- uses preProcess from Manifest to preprocess all the entries
preProc		:: (String, MetaValue) -> (String, MetaValue)
preProc (k, v)
 = let (k', f)	= findWithDefault (k, id) k preProcess in
		(k', f v)

-- typechecks against given types in manifest.hs
typeCheck	:: (String, MetaValue) -> Exc (Bool, (String, MetaValue))
typeCheck (k, v)
 	= do	let t	= findWithDefault v k typeOf	-- typeOf is defined in manifest.hs
		let typeOk	= v `hasType` t
		unless typeOk $ err $ "The field '"++k++"' should be of the type "++showMetaType t++",\nbut it is of the type "++showMetaType v
		return (typeOk, (k,v))

-- extension of preProcess from manifest.hs
preProcess'	= fromList [("license",("license", (|> License) . parseLicense))]

preProc'	:: (String, MetaValue) -> Exc (String, MetaValue)
preProc' (k,v)
	= do	let (k', f)	= findWithDefault (k, return) k preProcess'
		v'	<- f v
		return (k', v')

preProcs'	:: [(String, MetaValue)] -> Exc [(String, MetaValue)]
preProcs' pairs	=  mapM preProc' pairs



knownLicenses	= (toEnum 0 :: KnownLicense) & enumFrom |> show ||>> toLower

parseLicense	:: MetaValue -> Exc License
parseLicense (String str)
	= getLicense str
parseLicense (GlobId str)
	= getLicense str
parseLicense (FuncName str)
	= getLicense str
parseLicense (ModuleName [str])	-- the bnf parses global idents as ModuleNames with one item
	= getLicense str
parseLicense (License l)	= return l
parseLicense mv	= halt $ "The meta value "++show mv++" is not a valid license"

getLicense str
	= str |> toLower & (`elemIndex` knownLicenses)
		& (? msg str) |> toEnum |> Known
msg str	= "The license "++show str++" is not a known license. Please specify a file"
