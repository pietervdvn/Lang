module Languate.MaintenanceAccess.TestRender where

import StdDef
import State
import Languate.MarkUp
import Data.Map

import System.Directory

fp	= "/test"

t = do	dir	<- getCurrentDirectory
	renderClusterTo ((extend preproc html) (dir ++ fp ++ "/html")) cluster
	renderClusterTo (md (dir ++ fp ++ "/md")) cluster

back = NonImp $ InLink (Base "Back to all pages") "All pages"


preproc		:: RenderSettings -> RenderSettings
preproc rs	= addPreprocessor' (\mu -> parags [back, mu, back]) rs


cluster	= buildCluster [doc1,doc2,doc3, doc4]


mu = Titling (Seq [Base "Example file ", emph "with ", imp "all", code " structs"]) $ parags
	    [ Base "Hallo"
            , OrderedList [Base "Item", OrderedList [Base "More", Base "Nested", Base "Lists"], Base "Item"]
            , emph "Test emph"
            , imp "important"
            , code  "x = \"code\""
            , incorr "wrong info"
	    , code ">"
	    , code "<"
	    , notImportant "Not important"
	    , Incorr $ Base "Strikethrough?"
            , titling "Main item" $ Seq
			[ parag "Information"
			, parag "More information"
			, titling "SubItem" $ Base "Hi"
			, titling "SubItem 2" $ Base "Hi again"]
            , InLink (Seq [Base "Some", emph "link"]) "Doc 2"
	    , Embed "Doc 2"
	    , Embed "Doc3"
	    , inlink "SubDir/Doc4"
	        , Table [imp "Head 1", imp "Head 2"] [["Row 1","Row 1 again"] |> Base, [Base "Row 2", List [Base "Row 2 again", Base "Row 2 again"]]]
            , List [Base "Item", List [Base "More", Base "Nested", Base "Lists"], Base "Item"]
            ]


mu0	= Seq [Base "Hallo!", Embed "Doc3"]

doc1	= Doc "Doc1" "This is the first document" (fromList [("key", "value")]) mu
doc2	= Doc "Doc 2" "This is the second document" empty mu0
doc3	= Doc "Doc3" "The third document" empty $ Base "Contents of doc3"
doc4	= Doc "SubDir/Doc4" "The fourth doc" empty $ Base "Hi"
