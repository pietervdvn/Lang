module Languate.FQN where

{--

This module implements the FQN-datatype, which represents a fully qualified name.

--}
import StdDef


newtype Author	= Author Name
	deriving (Eq,Ord)
instance Show Author where
	show (Author auth)	= auth


newtype PackageName	= PackName Name
	deriving (Eq,Ord)
instance Show PackageName where
	show (PackName name)	= name

newtype ModuleName	= ModName Name
	deriving (Eq,Ord)
instance Show ModuleName where
	show (ModName name)	= name

-- fully qualified package name: author, packagename, version
data FQPN	= FQPN Author PackageName
	deriving (Eq,Ord)
instance Show FQPN where
	show (FQPN auth pack)
		= show auth++":"++show pack

-- fully qualified module name
data FQN	= FQN FQPN [ModuleName] ModuleName
	deriving (Eq,Ord)
instance Show FQN where
	show (FQN fqnp mods mod)
		= show fqnp++":"++foldr (\m acc -> show m++"."++acc) (show mod) mods


-- converts "Languate.Core.Blabla" -> "Languate/Core/Blabla"
relativePath	:: FQN -> FilePath
relativePath (FQN _ mods mod)
		= foldr (\m acc ->show m++"/"++acc) (show mod) mods

toFQPN		:: String -> Maybe FQPN
toFQPN fqpn	=  do	let (auth,pack)	= break (==':') fqpn
			toFqpn auth pack

toFQN		:: String -> Maybe FQN
toFQN fqn	=  do	let splitted	= splitOn (==':') fqn
			if length splitted == 3 then do
				let [auth, pack, modsStr] = splitted
				let mods	= splitOn (=='.') modsStr
				toFqn auth pack (init' mods) (last mods)
				else Nothing
toFqn'		:: FQPN -> [Name] -> Name -> Maybe FQN	
toFqn' fqpn mods mod
		= do	mods'	<- mapM toModName mods
			mod'	<- toModName mod
			return $ FQN fqpn mods' mod'

toFqn		:: Name -> Name -> [Name] -> Name -> Maybe FQN
toFqn auth pack mods mod
		= do	fqpn	<- toFqpn auth pack
			toFqn' fqpn mods mod
			

toFqpn		:: Name -> Name -> Maybe FQPN
toFqpn auth pck	= do	auth'	<- toAuthor auth
			pack'	<- toPackName pck
			return $ FQPN auth' pack'

-- an authors name consists of [a..zA..Z0..9 -], e.g. "Pieter Vander Vennet"
toAuthor	:: Name -> Maybe Author
toAuthor	= constrIfValid " -" Author
-- [a..zA..Z-]*, e.g. languate-core
toPackName	:: Name -> Maybe PackageName
toPackName	=  constrIfValid "-" PackName

toModName	:: Name -> Maybe ModuleName
toModName	=  constrIfValid "" ModName

-- is valid if all chars in the string are in [a..zA..Z]++the passed chars
valid		:: String -> Name -> Bool
valid extra	=  all (`elem` extra++['a'..'z']++['A'..'Z']++['0'..'9'])

constrIfValid	:: String -> (Name -> a) -> Name -> Maybe a
constrIfValid extra constr name
		= if valid extra name then Just $ constr name else Nothing


splitOn		:: (a -> Bool) -> [a] -> [[a]]
splitOn f []	=  []
splitOn f ls	=  let (h,t)	= break f ls in
			h:splitOn f (drop 1 t)
