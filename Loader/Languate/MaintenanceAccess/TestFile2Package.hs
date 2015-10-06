module Languate.MaintenanceAccess.TestFile2Package where

import Languate.File2Package
import Languate.FQN
import Data.Maybe

import Bnf hiding (toFQN)

import System.IO.Unsafe

help 	= "tl = testLoad; tl' = unsafePerformIO tl"

tl	= loadPackage' bnfs fp -- run with current directory Lang/Loader
tl'	= unsafePerformIO tl
fp	= "../workspace/Data/"

bnfs	= unsafePerformIO $ load "../Parser/bnf/Languate.bnf"
