module Languate.MaintenanceAccess.TestRender where

import StdDef

import Languate.MarkUp.MarkUp
t = do  putStrLn $ renderMD mu
        putStrLn $ renderHTML mu
 
mu = Seq [Link (Base "Abc") "Link"
            , Emph (Base "Test")]
