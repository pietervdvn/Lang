module Languate.MaintenanceAccess.TestRender where

import StdDef
import State
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.HTML
import Languate.MarkUp.MD
import Languate.MarkUp.Cluster
import Data.Map (fromList)

import Data.Map

import System.Directory

fp	= "/test"

t = do	dir	<- getCurrentDirectory
	renderClusterTo html (dir ++ fp ++ "/html") cluster
	renderClusterTo md (dir ++ fp ++ "/md") cluster

cluster	= buildCluster [doc1,doc2,doc3]

mu = Seq    [ Base "Hallo"
            , OrderedList [Base "Item", OrderedList [Base "More", Base "Nested", Base "Lists"], Base "Item"]
            , emph "Test"
            , imp "important"
            , code  "x = \"code\""
            , incorr "wrong info"
            , titling "Main item" $ Seq
			[ parag "Information"
			, parag "More information"
			, titling "SubItem" $ Base "Hi"
			, titling "SubItem 2" $ Base "Hi again"]
            , InLink (Seq [Base "Some", emph "link"]) "Doc2"
	    , Embed "Doc3"
	        , Table [imp "Head 1", imp "Head 2"] [["Row 1","Row 1 again"] |> Base, [Base "Row 2", List [Base "Row 2 again", Base "Row 2 again"]]]
            , List [Base "Item", List [Base "More", Base "Nested", Base "Lists"], Base "Item"]
            ]


mu0	= Base "Hallo!"

doc1	= Doc "Doc1" "This is the first document" (fromList [("key", "value")]) mu
doc2	= Doc "Doc2" "This is the second document" empty mu0
doc3	= Doc "Doc3" "The third document" empty $ Base "Contents of doc3"


