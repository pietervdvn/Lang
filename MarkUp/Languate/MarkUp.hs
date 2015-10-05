module Languate.MarkUp (module M, renderString) where

import Languate.MarkUp.MarkUp as M
import Languate.MarkUp.Cluster as M
import Languate.MarkUp.Doc as M
import Languate.MarkUp.Classes as M
import Languate.MarkUp.Options as M
import Languate.MarkUp.HTMLOptions as M
import Languate.MarkUp.MDOptions as M
import Languate.MarkUp.Css as M

-- Stub for reexporting markup

import StdDef
import Languate.MarkUp.MD
import State


renderString	:: MarkUp -> String
renderString mu	=  let	md	= mu & rewrite _links	in
			runstate (renderMD md) (MdContext 0 0) & fst

_links		:: MarkUp -> Maybe MarkUp
_links (Link mu _)
		= Just mu
_links (InLink mu _)
		= Just mu
_links (Embed nm)
		= Just $ Base ("(see "++nm++")")
_links _	= Nothing
