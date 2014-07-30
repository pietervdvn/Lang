module Languate.MaintenanceAccess.TestFile2Package where

import Languate.File2Package
import Languate.FQN
import Data.Maybe


-- t	= loadPackage' tfqn fp -- run with current directory Lang/Loader
tfqn	= fromJust $ toFQN "pietervdvn:Data:Prelude" -- imports Data.Bool and Data.Functor
fp	= "../workspace/Data/src/"
