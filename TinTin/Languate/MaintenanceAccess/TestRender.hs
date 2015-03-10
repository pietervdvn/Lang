module Languate.MaintenanceAccess.TestRender where

import StdDef
import State
import Languate.MarkUp.MarkUp

fp	= "test/"

t = do  writeFile (fp ++ "md/file0.md") $ runstate (renderMD $ rewrite (renderLink ".md") mu) 1 & fst
	writeFile (fp ++ "md/file1.md") $ runstate (renderMD mu0) 1 & fst
	writeFile (fp ++ "html/file0.html") $ runstate (renderHTML $ rewrite (renderLink ".html") mu) 1 & fst
	writeFile (fp ++ "html/file1.html") $ runstate (renderHTML mu0) 1 & fst


mu = Seq    [ Base "Hallo"
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
	        , Table [imp "Head 1", imp "Head 2"] ([["Row 1","Row 1 again"],["Row 2","Row 2 again"]] ||>> Base)
            ]


mu0	= Base "Hallo!"


renderLink  ::String -> MarkUp ->  Maybe MarkUp
renderLink ext (Link naam link)
            = Just $ Link naam $ link ++ ext
renderLink _ _
            = Nothing
