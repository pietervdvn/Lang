module Bnf.Loader.ExportCalculator (calcExports, calcImports, importOne', Exports, Imports) where

import Bnf.Meta.IOModule
import Bnf.BNF
import Bnf.FQN
import StdDef
import State
import Control.Monad

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Arrow
import Data.List (nub)
import Data.Tuple (swap)

{--

This module implements the export calculation.
Each module exports rules, some locally defined, some reexported rules.

This module calculates what each module exports, by keeping a set of 'currently exporting', and updating this set each time an imported set changes. (Fixpoint/worklist algo)

--}


type Exports	= Set (FQN, Name)
type Imports	= Set (FQN, Name)


-- calculate what each module exports in function of the other modules and imports
calcExports	:: [IOModule] -> Map FQN Exports
calcExports modules
			=  let (seed, context) = (initialMap modules, createContext modules) in
				snd $ runstate (calculateExports (S.fromList $ M.keys seed) context) seed


calcImports		:: IOModule -> Map FQN Exports -> Map Name FQN
calcImports (IOM fqn _ imports _) ctx
		=  let all 	= map (`importOne'` ctx) imports in
			M.fromList $ nub $ concat all 

importOne'	:: IOImport -> Map FQN Exports -> [(Name, FQN)]
importOne' imp
		=  importOne imp . fromJust . M.lookup (getFQN imp)

-- assumes set of exports corresponds is from the module this improt is handling
importOne	:: IOImport -> Exports -> [(Name, FQN)]
importOne imprt@(IOImport fqn _ mode terms _)
		= map swap . filter (isImported imprt . snd) . S.toList

isImported	:: IOImport -> Name -> Bool
isImported (IOImport _ _ mode terms _) name
	= fromMaybe (mode == BlackList) (lookup name terms)


-- the initial map where each fqn maps on locally exported rules
initialMap	:: [IOModule] -> Map FQN Exports
initialMap	=  M.fromList . map (getFqn &&& localExports)
createContext	:: [IOModule] -> Map FQN (IOModule, [FQN])
createContext modules
		= M.fromList $ map (`prepContext` modules) modules

prepContext	:: IOModule -> [IOModule] -> (FQN, (IOModule, [FQN]))
prepContext iom allModules
		= let fqn	= getFqn iom in 
			(fqn, (iom, map getFqn $ modulesImporting fqn allModules))

-- Names of modules which should be checked, 
-- a map of fqn --> it's module , what other modules it is used in , locally defined rules
-- the new state of exported stuff + what modules might be changed
-- We assume local rules already are added to the starting state, so we propagate all these now
calculateExports	:: Set FQN -> Map FQN (IOModule, [FQN]) -> State (Map FQN Exports) [FQN]
calculateExports todo context
	| S.null todo	= return []
	| otherwise
		= do	-- we export all the stuff that we should export. If the "we export"-set has changed, all modules which import this module should be notified and added (again) to the todo-list
			let fqn		= S.findMin todo
			let (iom, importedBy)= fromJust $ M.lookup fqn context
			currentExports	<- fmap (fromJust . M.lookup fqn) get
			exportContext	<- get
			let (changed, exports')	= runstate (addExports iom exportContext) currentExports
			let todo' = (if changed then S.union $ S.fromList importedBy else id) $ S.delete fqn todo
			when changed $ modify (M.insert fqn exports')	
			calculateExports todo' context		

addExports	:: IOModule -> Map FQN Exports -> State Exports Bool
addExports iom context
		= do	let imports	= getImports iom
			let exports	= S.unions $ map (neededExports' context) imports
			controlledAdds $ S.toList exports

neededExports'	:: Map FQN Exports -> IOImport ->  Exports
neededExports' context imprt
	| not $ M.member (getFQN imprt) context	= S.empty
	| otherwise	= neededExports imprt (fromJust $ M.lookup (getFQN imprt) context)

-- calculates exports for the given IO-module. Hereby does the fqn and exports-argument stand for the fqn and exports of the imported module. The return gives all exports that are imported from that module.
neededExports	:: IOImport -> Exports -> Exports
neededExports imprt@(IOImport fromModule doExport mode terms _) whichExports
	| not doExport			= S.empty
	| otherwise	= S.filter (exportAllowed (mode == BlackList) terms . snd) whichExports

modulesImporting :: FQN -> [IOModule] -> [IOModule]
modulesImporting fqn
		= filter (elem fqn . map getFQN . getImports)


exportAllowed	:: Bool -> [(Name, IsVisible)] -> Name -> Bool
exportAllowed allowIfUnstated graylist suspect
	= let isVisible = lookup suspect graylist in
		fromMaybe allowIfUnstated isVisible

controlledAdds	:: Ord a => [a] -> State (Set a) Bool
controlledAdds 	= fmap or . mapM controlledAdd

-- adds an export to the map, returns the new set. The boolean indicates wether the set has been changed
controlledAdd	:: Ord a => a -> State (Set a) Bool
controlledAdd toAdd
		= do	mem	<- fmap (not . S.member toAdd) get
			when mem $ modify $ S.insert toAdd
			return mem
