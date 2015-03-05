module Languate.MaintenanceAccess.TestRender where

import StdDef

import Languate.MarkUp.MarkUp
t = do  putStrLn $ renderMD mu
        putStrLn $ renderHTML mu

mu = Seq    [ Base "Hallo"
            , Emph (Base "Test")
            , Imp "important"
            , Code "x = \"code\""
            , Incorr "wrong info"
            , Titling "Main item" $ Seq
			[ Parag "Information"
			, Parag "More information"
			, Titling "SubItem" "Hi"
			, Titling "SubItem 2" "Hi again"]
            , Link (Seq [Base "Some", Emph "link"]) "Link"
            ]
