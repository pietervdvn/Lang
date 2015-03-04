module Languate.Manifest.CreateManifest where

{--
This modules creates a manifest from key-value pairs
--}

import StdDef

import Languate.Manifest.Manifest


createFromDict	:: Name -> String -> String -> [(String, MetaValue)] -> Manifest
createFromDict name synopsis description pairs
	= todos $ "Actual manifest "++name++synopsis++description++"\n"++ unlines (pairs |> show)
