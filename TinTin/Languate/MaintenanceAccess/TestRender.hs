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
            , Emph $ Base "Test"
            , Imp $ Base "important"
            , Code $ Base "x = \"code\""
            , Incorr $ Base "wrong info"
            , Titling (Base "Main item") $ Seq
			[ Parag $ Base "Information"
			, Parag $ Base "More information"
			, Titling (Base "SubItem") $ Base "Hi"
			, Titling (Base "SubItem 2") $ Base "Hi again"]
            , Link (Seq [Base "Some", Emph $ Base "link"]) "file1"
            ]


mu0	= Base "Hallo!"
 

renderLink  ::String -> MarkUp ->  Maybe MarkUp
renderLink ext (Link naam link)
            = Just $ Link naam $ link ++ ext
renderLink _ _ 
            = Nothing
