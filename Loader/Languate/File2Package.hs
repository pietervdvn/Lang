module Languate.File2Package (loadPackage, loadPackage', import2fqn) where

{--

This module loads a module, looks at it's imports and loads unloaded stuff

--}
import StdDef
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

import Languate.Manifest.ParseManifest (parseManifest)


-- loadpackage, but crashes when imports are not found
loadPackage'	:: Bnf.World -> FQN -> FilePath -> IO Package
loadPackage' world fqn fp
		= do	manifest		<- parseManifest world $ fp ++ "/Manifest"
			(package, notFound)	<- loadPackage world fqn $ fp ++ "/src/"
			unless (null notFound) $ printErr notFound
			return $ buildWorld manifest package

printErr	:: [(FQN,FQN)] -> IO ()
printErr notFound
		= do	putStrLn $ msg notFound
			error "Not all imported modules were found."

msg		:: [(FQN, FQN)] -> String
msg		= foldl (\acc (requestor, notF) -> acc++"\n\t"++show notF++" (needed by "++ show requestor++")") " The Following packages where not found:"

{- loads all modules needed for file. FQN is the name of the module that should be loaded, Filepath the path to 'src' in the project.
Imports of which the file was not found, are the second value in the tuple. It is a list, containing
[this module wanted the import, this module was not found]
-}
loadPackage	:: Bnf.World -> FQN -> FilePath -> IO (Map FQN (Module, Set (FQN, Import)),[(FQN,FQN)])
loadPackage bnfs fqn src
		=  do	let FQN fqpn _ _	= fqn
			let ctx	= Context bnfs fqpn [(fqn, fqn)] empty src []
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
