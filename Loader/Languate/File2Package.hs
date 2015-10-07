module Languate.File2Package (loadPackage, loadPackage', import2fqn) where

{--

This module loads a module, looks at it's imports and loads unloaded stuff

--}
import StdDef
import HumanUtils (commas, intercal)
import Exceptions
import qualified Bnf
import Languate.FQN
import Languate.AST
import Languate.File2AST
import Languate.Package
import Data.Map hiding (null, map, filter, foldr, foldl)
import StateT
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Directory
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Control.Arrow

import Languate.Manifest.ParseManifest (parseManifest)
import qualified Languate.Manifest as Mani
import Languate.Manifest hiding (fqpn)
import Languate.Manifest.Manifest hiding (fqpn)


-- loadpackage, but crashes when imports are not found
loadPackage'	:: Bnf.World ->FilePath -> IO Package
loadPackage' bnfs fp
		= loadPackage bnfs fp >>= runExceptionsIO'

loadPackage	:: Bnf.World -> FilePath -> IO (Exceptions' String Package)
loadPackage world fp
		= do	manifest		<- parseManifest world $ fp ++ "/Manifest"
			let packFQN		= Mani.fqpn manifest
			let convNames2FQNS nms	= nms |> (toFqn'' packFQN &&& id) & mapM _filterInvalid |> concat
			exposed			<- manifest & exposes & S.toList & convNames2FQNS
			maintained		<- manifest & rest & M.lookup "maintains" & fromMaybe (St [])
							& (\(St mods) -> mods) |> (\(ModuleName names) -> names) & convNames2FQNS
			(package, missing)	<- _loadPackage world (exposed++maintained) $ fp ++ "/src/"
			unless (null missing) $ printErr exposed maintained missing
			let srcPath		= fp ++ "/src"
			allFiles		<- getDirectoryContentsRecursive srcPath
			let allFiles'		= allFiles |> drop (1 + length srcPath)
			return (buildWorld manifest package >>= checkObsoleteModules exposed maintained allFiles')

checkObsoleteModules	:: [FQN] -> [FQN] -> [FilePath] -> Package -> Exceptions' String Package
checkObsoleteModules exposed maintained fps pack
		= do	let loadedMods = pack & modules & M.keys |> modulePath |> intercal "/"
			let obsolete	= fps L.\\ loadedMods
			assert (null obsolete) $ "Some packages are not used (and thus not loaded). Add them as 'maintains' in the manifest to load them too (and thus maintain them).\n"++exposedMaintained exposed maintained ++ "\nUnused files are: "++commas obsolete
			return pack

_filterInvalid :: (Maybe FQN, [Name]) -> IO [FQN]
_filterInvalid (Nothing, mods)
		= putStrLn ("Not a valid FQN in the manifest: "++ intercal "." mods) >> return []
_filterInvalid (Just fqn, _)
		= return [fqn]

printErr	:: [FQN] -> [FQN] -> [(FQN,FQN)] -> IO ()
printErr exposed maintained notFound
		= do	putStrLn $ msg notFound
			putStrLn $ exposedMaintained exposed maintained
			error "Not all imported modules were found."


exposedMaintained	:: [FQN] -> [FQN] -> String
exposedMaintained exposed maintaned
	=	let showFQNS fqns	= if null fqns then "no packages" else fqns |> show & commas in
		"The manifest exposes "++showFQNS exposed++" and maintains "++ showFQNS maintaned

msg		:: [(FQN, FQN)] -> String
msg fqns	= let	msgs	= fqns |> (\(requestor, missing) -> show missing++" (needed by "++ show requestor++")")
			msgs'	= msgs & unlines & indent in
			"The following packages where not found:" ++ msgs'


{- loads all modules needed for given filepath. FQN is the name of the module that should be loaded, Filepath the path to 'src' in the project.
Imports of which the file was not found, are the second value in the tuple. It is a list, containing
[this module wanted the import, this module was not found]
-}
_loadPackage	:: Bnf.World -> [FQN] -> FilePath -> IO (Map FQN (Module, Set (FQN, Import)),[(FQN,FQN)])
_loadPackage bnfs [] src
		= do	putStrLn "Put at least one module in 'exposes' or 'maintanance' in the manifest, otherwise no modules will be loaded!"
			return (M.empty, [])
_loadPackage bnfs fqns src
		=  do	let FQN fqpn _ _	= head fqns
			let ctx	= Context bnfs fqpn (fqns |> (id &&& id)) empty src []
			(_, ctx)	<- runstateT loadRec ctx
			let notF	= notFound ctx
			return (loaded ctx, notF)

-- left fqn imports the right fqn
type ToLoad	= [(FQN,FQN)]
type Loaded	= Map FQN (Module, Set (FQN, Import))

-- not found: requestor -> missing request
data Context	= Context { bnfs :: Bnf.World, fqpn :: FQPN, toLoad:: ToLoad, loaded:: Loaded, root::FilePath, notFound::[(FQN,FQN)]}

-- loads as long as there are things in the toLoad-list
loadRec	:: StateT Context IO ()
loadRec	=  do	done	<- get' $ null . toLoad
		unless done $ do 	loadNext
					loadRec

loadNext	:: StateT Context IO ()
loadNext	=  do	request	<- pop
			cached	<- get' $ member (snd request) . loaded
			unless cached $ uncurry loadF request

loadF		:: FQN -> FQN -> StateT Context IO ()
loadF requestor fqn
		=  do	fpr	<- get' root
			let fp	=  fpr ++ relativePath fqn
			exists	<- lift $ doesFileExist fp
			if not exists then modify $ addNotFound requestor fqn
				else do bnf	<- get' bnfs
					lift $ putStrLn $ "Loading '"++show fqn++"'"
					modul	<- lift $ load bnf fp
					cache	<- get' loaded
					imports	<- addImports fqn modul
					modify $ setLoaded $ insert fqn (modul,imports)  cache

-- adds all the imports to the toLoad-list. Returns the FQN's which are needed
addImports	:: FQN -> Module -> StateT Context IO (Set (FQN,Import))
addImports fqn mdul
		=  do	fqnp	<- get' fqpn
			cache	<- get' loaded
			let fqns 	= map (import2fqn fqnp) $ imports' mdul
			let fqns'	= zip (repeat fqn) $ filter (`notMember` cache) $ fmap fst fqns
			todolist	<- get' toLoad
			modify (setToLoad $ fqns' ++ todolist)
			return $ S.fromList fqns




import2fqn	:: FQPN -> Import -> (FQN, Import)
import2fqn fqpn imp@(Import _ mods mod _ _)
		= (fromMaybe (error $ "Invalid import format "++show mods ++ show mod) $ toFqn' fqpn mods mod, imp)

pop		:: StateT Context IO (FQN, FQN)
pop		=  do	ls	<- get' toLoad
			modify (setToLoad $ tail ls)
			return $ head ls


getDirectoryContentsRecursive	:: FilePath -> IO [FilePath]
getDirectoryContentsRecursive fp
	= do	isDir	<- doesDirectoryExist fp
		if not isDir then return [fp]
		else do	fps'	<- getDirectoryContents fp |> (L.\\ [".",".."]) ||>> ("/"++) ||>> (fp ++)
			mapM getDirectoryContentsRecursive fps' |> concat

-- # Helper functions for lensing...

setLoaded	:: Loaded -> Context -> Context
setLoaded loaded (Context bnfs fqpn toLoad _ root errs)
		= Context bnfs fqpn toLoad loaded root errs

setToLoad	:: ToLoad -> Context -> Context
setToLoad toLoad (Context bnfs fqpn _ loaded root errs)
		= Context bnfs fqpn toLoad loaded root errs

addNotFound	:: FQN -> FQN -> Context -> Context
addNotFound requestor fqn (Context bnfs fqpn toLoad loaded root errs)
		= Context bnfs fqpn toLoad loaded root $ (requestor, fqn):errs
