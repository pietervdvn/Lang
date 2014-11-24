module Languate.AST.ModuleAST where

{--

This module implements the data structures representing statements and modules.

--}

import StdDef
import Languate.AST.TypeAST
import Languate.AST.TypeASTUtils
import Languate.AST.FunctionAST
import Languate.AST.FunctionASTUtils

import Data.Either (rights)

data Module	= Module {moduleName::Name, exports::Restrict, imports::Imports, statements::[Statement]}
	deriving (Show)

-- ## Stuf about imports

-- for comments hovering around imports
type Imports	= [Either Comment Import]

imports'	:: Module -> [Import]
imports' 	=  rights . imports


type Pseudonym	= Name
-- represents an import statement. public - Path - ModuleName - pseudonym = name as which the module has been imported - restrictions
data Import	= Import Visible [Name] Name (Maybe Pseudonym) Restrict
	deriving (Show)
-- restrict is the blacklist/whitelist of the showing/hiding in an import statement. Can contain both function/operator names and type names
data Restrict	= BlackList [Name] | WhiteList [Name]
	deriving (Show)


data Statement	= FunctionStm 	Function
		| ADTDefStm	ADTDef
		| SynDefStm	SynDef
		| SubDefStm	SubDef
		| ClassDefStm	ClassDef
		| InstanceStm 	Instance
		| Comments [Comment]
		| ExampleStm	Law
		| AnnotationStm	Annotation
	deriving (Show)

isAllowed	:: Restrict -> Name -> Bool
isAllowed (BlackList items)
		= not . (`elem` items)
isAllowed (WhiteList items)
		= (`elem` items)
