module Languate.FunctionTable.BuildDocstringTable where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.Package
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.FunctionTable

import Data.Maybe
import Data.Map as M
import Data.List as L

import Debug.Trace


buildDocstringTable	:: Package -> FunctionTables -> Exc DocstringTable
buildDocstringTable p fts
	= dictMapM (buildDocstringTableFor p) (unpackFTS fts) |> M.toList ||>> snd |> unions


buildDocstringTableFor	:: Package -> FQN -> FunctionTable -> Exc DocstringTable
buildDocstringTableFor p fqn ft
	= do	modul	<- (modules p & M.lookup fqn) ? ("Module "++show fqn++" not found")
		ft & defined & mapWithKey (fetchDocstring modul) & return

fetchDocstring	:: Module -> Signature -> Coor -> String
fetchDocstring m sign coor
		= [ docStrings m & L.filter ((==) (signName sign) . snd . fst) & listToMaybe |> snd,	-- TODO better selection!
 		    searchCommentAbove' m coor]
			& L.foldr firstJust Nothing & fromMaybe ""
