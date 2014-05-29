module Bnf.Meta.Pt2Module where

{--
This module implements the pt -> AST conversion for a bnf - module
--}

import Control.Monad.Writer
import StdDef
import Bnf.Converter
import Bnf.ParseTree

import Bnf.Meta.Pt2Meta (parseMeta)
import Bnf.Meta.Pt2Rule (parseRule)
import Bnf.Meta.Pt2Import (parseImport)
import Bnf.Meta.Pt2FQN (parseFQN)
import Bnf.Meta.IOModule
import Bnf.FQN
import Data.Maybe (fromJust)

parseModule	:: ParseTree -> Writer Errors IOModule
parseModule pt	=  do	parsed	<- simpleConvert h t s $ cleanAll ["comment","nl"] pt
			conv parsed


conv		:: AST -> Writer Errors IOModule
conv (Module fqn metas imports rules)
		= do	checkMeta fqn metas
			return $ IOM fqn metas imports rules

checkMeta	:: FQN -> [IOMeta] -> Writer Errors ()
checkMeta fqn metas	
	=  do	let names	= map (\(n, _, _) -> n) metas
		let notFound	= filter (not . flip elem names) ["author","desc","date"]
		err (metaNotFound fqn) notFound
		let double	= allDoubles names
		let sndPos	= reverse $ map unsnd metas
		err (metaDouble fqn sndPos) double
		let datas	= map unthrd metas
		when ("date" `elem` names)
			(do	let date	= fromJust $ lookup "date" datas
				let (l,c)	= fromJust $ lookup "date" sndPos
				case date of
					Left _	-> tellErr (fqn,"date:type",(0,l,c)) "Date metainfo expeced a list of (at least) 5 ints"
					Right time	-> checkDatum (l,c) fqn time
				)

checkDatum	:: Position -> FQN -> [Either Int String] -> Writer Errors ()
checkDatum (l,c) fqn time	
		=  do	let areLeft	= map isLeft time
			unless (and areLeft) $ tellErr (fqn, "meta:datetype", (0,l,c))
				"Expected a list of ints for the date metainfo"
			let len		= length time
			when (len < 6) $ tellErr (fqn, "meta:date", (0,l,c))
				"Need at leat 5 ints for the date meta info [year month day hour minute], some are missing"
			return ()

isLeft		:: Either a b -> Bool
isLeft (Left _)	= True
isLeft _	= False

allDoubles	:: Eq a => [a] -> [a]
allDoubles (a:as)
	| a `elem` as	= a:allDoubles (filter (a /=) as)
	| otherwise	= allDoubles as
allDoubles []		= []

unsnd	:: (a,b,c) -> (a,c)
unsnd (a,_,c)
	= (a,c)

unthrd	:: (a,b,c) -> (a,b)
unthrd (a,b,_)	= (a,b)

err		:: (Name -> Error) -> [Name] -> Writer Errors ()
err f		=  mapM_ (\n -> tell [Right $ f n])

metaNotFound	:: FQN -> Name -> Error
metaNotFound fqn name
		=((fqn, "meta:obligated", (0,0,0)),"Expected metainfo with name '"++name++"'")

metaDouble	:: FQN -> [(Name,Position)] -> Name -> Error
metaDouble fqn infs name
		= let (l,c) = fromJust $ lookup name infs in
			((fqn, "meta:double", (0,l,c)), "Metainfo with name '"++name++"' already defined")

tellErr		:: RuleInfo -> String -> Writer Errors ()
tellErr inf msg	=  tell [Right (inf, msg)]

data AST	= Module FQN [IOMeta] [IOImport] [IORule]
		| Fqn FQN
		| Meta [IOMeta]
		| Import [IOImport]
		| Rule [IORule]
	deriving (Show)

h		:: Name -> ParseTree -> Maybe (Writer Errors AST)
h "rule"	=  pHook parseRule Rule
h "moduleName"
		= pHook parseFQN (\[fqn] -> Fqn fqn)
h "metafield"
		=  pHook parseMeta Meta
h "import"	=  pHook parseImport Import

h _ 		=  const Nothing

pHook		:: (ParseTree -> Writer Errors a) -> ([a] -> AST) -> ParseTree -> Maybe (Writer Errors AST)
pHook parser constructor pt
		=  Just $ do	parsed	<- parser pt
				return $ constructor [parsed]


t		:: Name -> String -> AST
t nm str	=  error $ "[Pt2Module] Token fallthrough for '"++nm++"' with "++show str


s		:: Name -> [AST] -> AST


s _ [ast]	= ast
s "module" [Fqn fqn, Meta metas, Import imports, Rule rules]
		= Module fqn metas imports rules
s "module" [Fqn fqn, Meta metas, Rule rules]
		= Module fqn metas [] rules
s "meta" (Meta head:rest)
		= let Meta tail = s "meta" rest in
			Meta $ head ++ tail
s "rules" iorules
		=  unpck iorules []
s nm items	=  error $ "[Pt2Module] Seq fallthrough for '"++nm++"' with "++show items

unpck		:: [AST] -> [IORule] -> AST
unpck [] acc	=  Rule acc
unpck (Rule ior:rs) acc
		= unpck rs (ior++acc)


