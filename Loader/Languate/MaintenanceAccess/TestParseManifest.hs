module Languate.MaintenanceAccess.TestParseManifest where

import Languate.Manifest

import qualified Bnf

fp	= "../workspace/Data/Manifest"
bnfpath	= "../Parser/bnf/Manifest.bnf"
bnf	= Bnf.load bnfpath

t	= do	bnf'	<- bnf
		parseManifest bnf' fp
