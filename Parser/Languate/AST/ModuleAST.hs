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

data Module	= Module {moduleName::Name, exports::Restrict, imports::Imports, statements'::[(Statement, Coor)]}
	deriving (Show)

-- ## Stuf about imports

-- for comments hovering around imports
type Imports	= [Either Comment (Import, Coor)]

type Pseudonym	= Name
-- represents an import statement. public - Path - ModuleName - pseudonym = name as which the module has been imported - restrictions
data Import	= Import Visible [Name] Name (Maybe Pseudonym) Restrict
	deriving (Show, Ord, Eq)
-- restrict is the blacklist/whitelist of the showing/hiding in an import statement. Can contain both function/operator names and type names
data Restrict	= BlackList [Name] | WhiteList [Name]
	deriving (Show, Ord, Eq)


data Statement	= FunctionStm 	Function
		| LawStm	Law
		| ADTDefStm	ADTDef
		| SubDefStm	SubDef
		| ClassDefStm	ClassDef
		| InstanceStm 	Instance
		| Comments [Comment]
		| DocStringStm [DocString (Name, Name)]	-- a docstringstatement (in the declaration from "Type"), about "function"
		| AnnotationStm	Annotation
		| PrecedenceStm PrecedenceAnnot
	deriving (Show)
