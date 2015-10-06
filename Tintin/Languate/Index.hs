module Languate.Index where

import StdDef
import HumanUtils
import Languate.MarkUp as Mu
import Languate.Package as P
import Languate.Manifest as Mani
import Languate.FQN
import Languate.TableOverview
import Languate.Manifest2Doc


index	:: Package -> TableOverview -> Doc
index p to
	= let	manif	= manifest p
		fqpn	= Mani.fqpn manif
		version	= Mani.version manif |> show & intercal "."
		in
 	 	doc ("Index of "++show fqpn++" "++version) ("The home of the "++Mani.name manif++" package") $
			titling "Overview" $ Seq [Parag $ imp $ synopsis manif, parag $ Mani.description manif, Embed manifestDocTitle]
