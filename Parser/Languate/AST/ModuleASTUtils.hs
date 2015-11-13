module Languate.AST.ModuleASTUtils where

import StdDef

import Languate.AST.TypeAST
import Languate.AST.ModuleAST
import Languate.AST.FunctionAST
import Normalizable


import Data.Either
import Data.Maybe (mapMaybe, listToMaybe, catMaybes)
import Control.Arrow
import Control.Applicative

setname	:: Name -> Module -> Module
setname name (Module _ restrict imps stms)
		= Module name restrict imps stms

setExports	:: [Name] -> Module -> Module
setExports names (Module name _ imps stms)
		= Module name (WhiteList names) imps stms

modImports		:: (Imports -> Imports) -> Module -> Module
modImports f (Module name restrict imps stms)
		= Module name restrict (f imps) stms

addStm		:: Coor -> Statement -> Module -> Module
addStm _ (Comments []) mod
		= mod
addStm coor stm (Module name restrict imps stms)
		= Module name restrict imps ((stm,coor):stms)


addStms		:: [(Statement,Coor)] -> Module -> Module
addStms stms mod
		= foldr (uncurry $ flip addStm) mod stms


statements	= map fst . statements'

-- Given a restricted list, is it a public function?
isAllowed	:: Restrict -> Name -> Bool
isAllowed (BlackList items)
		= not . (`elem` items)
isAllowed (WhiteList items)
		= (`elem` items)

isAllowed'	:: Restrict -> Name -> Visible
isAllowed' restr n
		= if isAllowed restr n then Public else Private

isPublic	:: Visible -> Bool
isPublic Public	= True
isPublic _ 	= False

toVisible	:: Bool -> Visible
toVisible True	= Public
toVisible False	= Private


isBlacklist	:: Restrict -> Bool
isBlacklist (BlackList _)	= True
isBlacklist _	= False

isWhitelist	= not . isBlacklist


imports''	:: Module -> [(Import, Coor)]
imports'' 	=  rights . imports

imports'	:: Module -> [Import]
imports'	=  map fst . imports''


docStrings	:: Module -> [((Name, Name), Comment)]
docStrings	=  concatMap isDocstr . statements

isDocstr	:: Statement -> [((Name, Name), Comment)]
isDocstr (DocStringStm docs)
		= map (about &&& comment) docs
isDocstr _	= []

-- Docstrings for consructors in ADTs, or for functions within categories (Typename, functionname)
docstringFor	:: Module -> (Name,Name) -> Maybe Comment
docstringFor m ident
		= lookup ident $ docStrings m


-- Searches the comment just above the declaration you asked for
searchCommentAbove	:: Module -> (Statement -> Bool) -> Maybe Comment
searchCommentAbove m n 	= _sca (statements m) n Nothing

-- Searches the comment just above the given coordinate
searchCommentAbove'	:: Module -> Coor -> Maybe Comment
searchCommentAbove' m c	= statements' m & break ((==) c . snd) & fst	-- take the statements before the coordinate
				|> fst & reverse |> commentIn & catMaybes & listToMaybe

_sca		:: [Statement] -> (Statement -> Bool) -> Maybe Comment -> Maybe Comment
_sca (stm:stms) f lastComment
		= if f stm then lastComment
			else _sca stms f $ firstJust (commentIn stm) lastComment

commentIn	:: Statement -> Maybe Comment
commentIn (Comments cs)
		= listToMaybe $ reverse cs
commentIn _	= Nothing



declaresType	:: Name -> Statement -> Bool
declaresType nm (ADTDefStm (ADTDef nm' _ _ _ _))
		= nm == nm'
declaresType nm (SubDefStm (SubDef nm' _ _ _ _))
		= nm == nm'
declaresType nm (ClassDefStm cd)
		= nm == name cd
declaresType _ _
		= False

isTypeDefStm	:: Statement -> Bool
isTypeDefStm (ADTDefStm{})
		= True
isTypeDefStm _	= False


instance Normalizable Statement where
	normalize	= ns


ns (Comments coms)
	= Comments $ filter (/= "") coms
ns (DocStringStm docs)
	= DocStringStm $ filter ((/=) "" . comment) docs
ns stm	= stm


nss	:: [Statement] -> [Statement]
nss	= filter hasContent

-- Returns True if the statement has usefull content. E.g. empty comments/docs will return false
hasContent	:: Statement -> Bool
hasContent (Comments [])
		= False
hasContent (DocStringStm [])
		= False
hasContent _	= True
