module Haskell where

{--

This module setups the file structure for a languate project, in a given location, (with readme initialization) 

--}


init	:: FilePath -> IO ()
init fp	=  do	print "Initializing project at "++show fp
