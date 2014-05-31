module Bnf.Loader.Convertor (convert) where

import Bnf.Meta.IOModule
import Bnf.BNF
import Bnf.FQN
import StdDef

import Data.Map (Map, fromList, member)
import qualified Data.Map as M
import Bnf.Loader.ExportCalculator
import Bnf.Loader.Check
import Data.Maybe
import qualified Data.Set as S
import Data.List (nub)

import Regex.Def hiding (Seq)
import qualified Regex.Def as RGX
{--

This module converts the IO-module-list, loaded by loader into a world. It solves cross-module dependencies.


This module closes the loop.
https://www.youtube.com/watch?v=gY2VADkYanQ. 

--}

convert	:: [(IOModule, FilePath)] -> World
convert modules
	=  let context = calcExports $ map fst $ modules in
		M.fromList $ map (\m -> (getFqn $ fst m, convertOne context m) ) modules

convertOne	:: Map FQN Exports -> (IOModule, FilePath) -> Module
convertOne context (iom, fp)
	= let imps = calcImports iom context in
		check' context fp iom $ Module (localRules (getFqn iom) (M.keys imps) iom) imps




dubbleImpErrMsg	:: [FQN] -> String
dubbleImpErrMsg	fqns
		= "Some modules are imported more then once:" ++ 
			foldr (\fqn acc -> "\n\t"++show fqn++acc) "" fqns



dubblesErrMsg	:: [Name] -> [(Name, FQN)] -> String
dubblesErrMsg dubbles imps
		= "Some terms are found in more then one module:"++
			foldr (\term acc -> "\n\t" ++ term++" is found in "++ 
				(show $ occursIn term imps) ++ acc) "" (nub dubbles)

occursIn	:: Name -> [(Name, FQN)] -> [FQN]
occursIn name	=  map snd . filter ((==) name . fst)



tdata0 = [IOM  (FQN ["exampleBnf"] "Test1")  [("author",Left (Right "Pieter Vander Vennet"),(2,1)),("date",Right [Left 2013,Left 12,Left 23,Left 1,Left 18,Left 42],(3,1)),("desc",Left (Right "Testing bnf"),(4,1))] [IOImport  (FQN ["exampleBnf"] "Test2")  False BlackList [] (6,1)] [IORule "b" (Seq [Call "a"]) True False (8,2)],IOM  (FQN ["exampleBnf"] "Test2")  [("author",Left (Right "Pieter Vander Vennet"),(2,1)),("date",Right [Left 2013,Left 12,Left 23,Left 1,Left 18,Left 42],(3,1)),("desc",Left (Right "The syntaxis of tests!"),(4,1))] [IOImport  (FQN ["exampleBnf"] "Test1")  True BlackList [] (6,1)] [IORule "a" (Seq [Call "b"]) True False (8,2)]]

tdata1	= [
    IOM (FQN [] "Languate")
	[	("author",Left (Right "Pieter Vander Vennet"),(2,1)),
		("date",Right [Left 2014,Left 5,Left 29,Left 21,Left 48,Left 42],(3,1)),
		("desc",Left (Right "The syntaxis of the Languate programming language"),(4,1))] 
	[IOImport (FQN [] "Primitives") True BlackList [] (6,1)]
	[IORule "lang" (Seq [Call "nat",Call "nat"]) False False (8,1)],
   IOM (FQN [] "Primitives") 
	[	("author",Left (Right "Pieter Vander Vennet"),(2,1)),
		("date",Right [Left 2014,Left 5,Left 29,Left 21,Left 48,Left 42],(3,1)),
		("desc",Left (Right "The syntaxis of primitives in languate: Strings, ints and floats"),(4,1))]
	[]
	[IORule "nat" (Choice [Seq [Rgx (RGX.Seq [Fixed '0',Fixed 'x',MinTimes 1 (Or [Range '0' '9',Range 'a' 'f',Range 'A' 'F'])])],
			Choice [Seq [More (Rgx (RGX.Seq [Fixed '0',Fixed 'b',MinTimes 1 (Or [Fixed '0',Fixed '1'])]))],Seq [Rgx (MinTimes 1 (Range '0' '9'))]]]) 
	       True False (12,1),
	IORule "binprefix" (Seq [Rgx (RGX.Seq [Fixed '0',Fixed 'b'])]) 
	       False False (11,1),
	IORule "bindigit" (Seq [Rgx (Or [Fixed '0',Fixed '1'])]) False False (10,1),
	IORule "hexprefix" (Seq [Rgx (RGX.Seq [Fixed '0',Fixed 'x'])]) False False (9,1),
	IORule "hexdigit" (Seq [Rgx (Or [Range '0' '9',Range 'a' 'f',Range 'A' 'F',Fixed '\''])]) False False (8,1),
	IORule "digit" (Seq [Rgx (Or [Range '0' '9',Fixed '\''])]) False False (7,1)]]



