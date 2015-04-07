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

fp	= "test/"

t = do	renderClusterTo html (fp ++ "/html") cluster
	renderClusterTo md (fp ++ "/md") cluster
{-
do  writeFile (fp ++ "md/file0.md") $ runstate (renderMD $ rewrite (renderLink ".md") mu) (MdContext 1 0) & fst
	writeFile (fp ++ "md/file1.md") $ runstate (renderMD mu0) (MdContext 1 0) & fst
	writeFile (fp ++ "html/file0.html") $ renderDoc2HTML doc1
	writeFile (fp ++ "html/file1.html") $ renderDoc2HTML doc2
-}
cluster	= buildCluster [doc1,doc2]

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
            , Link (Seq [Base "Some", emph "link"]) "file1"
	        , Table [imp "Head 1", imp "Head 2"] [["Row 1","Row 1 again"] |> Base, [Base "Row 2", List [Base "Row 2 again", Base "Row 2 again"]]]
            , List [Base "Item", List [Base "More", Base "Nested", Base "Lists"], Base "Item"]
            ]


mu0	= Base "Hallo!"

doc1	= Doc "Doc1" "This is the first document" (fromList [("key", "value")]) mu
doc2	= Doc "Doc2" "This is the second document" empty mu0


renderLink  ::String -> MarkUp ->  Maybe MarkUp
renderLink ext (Link naam link)
            = Just $ Link naam $ link ++ ext
renderLink _ _
            = Nothing
