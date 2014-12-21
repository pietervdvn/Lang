module Languate.MaintenanceAccess.TestFile2Package where

import Languate.File2Package
import Languate.FQN
import Data.Maybe

import Bnf hiding (toFQN)

import System.IO.Unsafe


tl	= loadPackage' bnfs tfqn fp -- run with current directory Lang/Loader
tl'	= unsafePerformIO tl
tfqn	= fromJust $ toFQN "pietervdvn:Data:Prelude" -- imports Data.Bool and Data.Functor
fp	= "../workspace/Data/src/"

bnfs	= unsafePerformIO $ load "../Parser/bnf/Languate.bnf"
