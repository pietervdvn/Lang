module Languate.AST
	(Law (Law, Example), Comment, DocString, Annotation (Annotation, PrecAnnot)
	, PrecModifier (PrecLeft , PrecRight , PrecPrefix , PrecPostfix)
	, PrecRelation (PrecEQ, PrecLT)
	, Visible (Private, Public)
	, Expression (Nat, Flt, Chr, Seq, Tuple, BuiltIn, Cast, AutoCast, Call, Operator, ExpNl)
	, Type (Normal, Free, Applied, Curry, TupleType, Infer)
	, TypeRequirement, ADTDef (ADTDef), ADTSum (ADTSum), SynDef (SynDef), SubDef (SubDef)
	, ClassDef (ClassDef), Instance (Instance)
	, Function (Function), Clause (Clause), Pattern (Assign, Let, Deconstruct, Multi, Eval, DontCare, MultiDontCare)
	, Module (Module), Imports, Import (Import), Restrict (BlackList, WhiteList)
	, Statement (FunctionStm, ADTDefStm, SynDefStm, SubDefStm, ClassDefStm, InstanceStm, Comments, ExampleStm, AnnotationStm)
	, docstr, visibility, signs, reqs, laws, clauses
	, moduleName, exports, imports, statements, imports'
	, lawName, lawDeclarations, typeReqs, expr1, expr2
	, name, frees, classReqs, subclassFrom, classdocstr, classlaws, decls
	)




where

{--

The AST (abstract syntax tree) is the data structure which represents a part of the source code.
As these are lots of definitions and convenience methods, the implementation is spread out over multiple modules.
This module is but the stub, reexporting them to the outside world.


It is possible to recreate the source code starting from an AST like this (except for some whitespace that's moved and a few dropped comments (but most should be preserverd)

The data flow is:

String -> parsetree -> cpt (often called ast in a pt2...) -> ast (abstract syntax tree -- here defined data structure) -> semantic analysis -> Interpreter

The structures here include comments (except those within expressions); the data structures preserve order. These data structures are thus a good starting point for doc generation (but you'll have to sugar a little again, e.g. NormalType List -> [a], a:b:c:#empty -> [a,b,c] ...

--}

import Languate.AST.FunctionAST
import Languate.AST.FunctionASTUtils
import Languate.AST.ModuleAST
import Languate.AST.TypeAST
import Languate.AST.TypeASTUtils
