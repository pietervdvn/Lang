module Bnf.Meta.Pt2Import where

{--
This module converts a pt to an IOModule.Importstatement
--}

import StdDef
import Bnf.FQN
import Bnf.ParseTree
import Bnf.Converter
import Bnf.Meta.Pt2FQN (parseFQN)
import Bnf.Meta.IOModule
import Control.Monad.Writer

parseImport	:: ParseTree -> Writer Errors IOImport
parseImport pt	=  do	conved	<- simpleConvert h t s pt
			return $ conv (getPosition pt) conved


conv		:: Position -> AST -> IOImport
conv pos (Public imprt)
		=  let (IOImport fqn _ mode showing _) = conv pos imprt in
			IOImport fqn True mode showing pos
conv pos (Imp fqn hides)
		=  let (mode, list)	= getHiding hides in
			IOImport fqn False mode list pos


getHiding	:: AST -> (IOImportMode, [(Name, IsVisible)])
getHiding (Hiding names)
		= (BlackList, zip names $ repeat False)
getHiding (Showing names)
		= (WhiteList, zip names $ repeat True)

data AST	= Imp FQN AST
		| ImpName FQN
		| Public AST
		| Ident Name
		| Set [Name]
		| Hiding [Name]
		| Showing [Name]
		| PublicT
		| ImportT
		| HidingT	| ShowingT
		| SetO	| SetC	| Comma
	deriving (Show)


h		:: Name -> ParseTree -> Maybe (Writer Errors AST)
h "moduleName" pt	= Just $ do	fqn	<- parseFQN pt
					return $ ImpName fqn
h _ _		= Nothing

t 		:: Name -> String -> AST
t "import" "public"	= PublicT
t "import" "import"	= ImportT

t "hider" "hiding"	= HidingT
t "hider" "showing"	= ShowingT
t "localIdent" name	= Ident name
t _ "{"		= SetO
t _ "}"		= SetC
t _ ","		= Comma

t nm str	= error $ "Tkn fallthrough on "++nm ++" with "++show str


s		:: Name -> [AST] -> AST

s "import" (PublicT:rest)
		= Public $ s "import" rest
s "import" (ImportT:ImpName fqn:rest)
		= Imp fqn $ s "import" rest
s "import" []	= Hiding []

s "hider" (Ident name:Comma:rest)
		= s "hider" (Ident name:rest)
s "hider" []	= Set []
s "hider" [Ident head]
		= Set [head]
s "hider" (Set head:rest)
		= let Set tail = s "hider" rest in
			Set $ head++tail
s "hider" [HidingT, SetO, Set names, SetC]
		= Hiding names
s "hider" [ShowingT, SetO, Set names, SetC]
		= Showing names
s _ [ast]	= ast
s nm items	=  error $ "Sq fallthrought on "++nm++" with "++show items
