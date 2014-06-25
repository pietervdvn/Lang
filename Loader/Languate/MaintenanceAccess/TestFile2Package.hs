module Languate.MaintenanceAccess.TestFile2Package where

import Languate.File2Package
import Languate.FQN
import Data.Maybe

tfqn	= fromJust $ toFQN "pietervdvn:Data:Prelude" -- imports Data.Bool and Data.Functor
fp	= "../workspace/Data/src/"
