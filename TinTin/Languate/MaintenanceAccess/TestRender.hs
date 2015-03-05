module Languate.MaintenanceAccess.TestRender where

import StdDef
import State
import Languate.MarkUp.MarkUp
t = do  putStrLn $ runstate (renderMD mu) 1 & fst
        putStrLn $ runstate (renderHTML mu) 1 & fst

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
            , Link (Seq [Base "Some", Emph $ Base "link"]) "Link"
            ]
