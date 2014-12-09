module Langate.ImportTable.BuildAliasses where


-- builds aliasses based on imports
aliasses	:: Module -> Map FQN Name
aliasses md	=  let	imps	= imports' md in
