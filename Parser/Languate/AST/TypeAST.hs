module Languate.AST.TypeAST where

{--

This module implements the AST-data structures of all type related and meta things:

Types themself, ADT-definitions, class definitions, instance declarations and subtype declarations.

Notice: all utility functions (show, normalize, ...) are implemented in TypeASTUtils.

It also contains meta things, as laws, comments, docstrings
--}

import StdDef

-- ## META STUFF
data Law	= Law 	{ lawName		:: Maybe Name
			, lawDeclarations	:: [(Name, Maybe Type)]
			, typeReqs		:: [TypeRequirement]
			, expr1 		:: Expression
			, expr2 		:: Expression }
		| Example (Maybe Name) Expression Expression	-- can be interpreted easily
	deriving (Ord, Eq)

type Comment	= String
-- a comment just before any declaration, (thus with no newlines in between)
type DocString	= Comment


data Annotation	= Annotation Name String	-- a 'normal' annotation. See docs in the BNF
		| PrecAnnot {operator::Name, modif::PrecModifier, relations::[PrecRelation]}

data PrecModifier	= PrecLeft | PrecRight | PrecPrefix | PrecPostfix
	deriving (Eq)

data PrecRelation	= PrecEQ Name Name
			| PrecLT Name Name


-- ## EXPRESSIONS

data Visible	= Private
		| Public
	deriving (Show, Eq, Ord)


data Expression	= Nat Int
		| Flt Float
		| Chr Char
		| Seq [Expression]
		| Tuple [Expression]
		| BuiltIn String	-- Calls a built-in function, e.g. 'toString',
		| Cast Type	| AutoCast
		| Call String
		| Operator String
		| ExpNl (Maybe Comment)	-- a newline in the expression, which might have a comment
	deriving (Ord, Eq)


-- ## TYPE STUFF

-- The data structure representing a type in Languate. Probably one of the most important definitions!
data Type	= Normal String	-- A normal type, e.g. Bool
		| Free String	-- A 'free' type, such as 'a', 'b'. (e.g. in id : a -> a)
		| Applied Type [Type]
		| Curry [Type]
		| TupleType [Type]
		| Infer
	deriving (Eq, Ord)

{- The data structure representing a type requirement of the form "a has a supertype Ord".
When no special requirement is present, nothing is used.
-}
type TypeRequirement	= (Name, Type)



-- ADTDefs --
-------------

{-
Represents an ADT in languate.
> data List a  = Cons a (List a) | Nil
becomes : ADTDef "List" [("a", Nothing)] "Comment about a list" product
> data NaiveDict a b	= [a,b]
> data BalancedTreeDict (a in Eq) b	= ...
becomes
ADTDef "Dict" ["k","v"] [("k","Ord")] "Docstring blabla" product
-}
data ADTDef	= ADTDef Name [Name] [TypeRequirement] DocString [ADTSum]


{-
A single constructor is represented by a single ADTSum-object.
The needed types are the last argument, they might (or might not) have a name
e.g.
> data ADT	= A x:Int Float	-- docstring for A
ADTSum "A" Visible (Just "docstring for A") [(Just "x", "Int"),(Nothing, "Float")]
-}
data ADTSum	= ADTSum Name Visible (Maybe Comment) [(Maybe Name, Type)]


-- Synonym and subtyping --
---------------------------

{-
> type Name = String.
> type Set (a in Ord)	= {a}
  SynDef "Set" ["a"] (Applied (Normal "Set") (Free "a")) [("a"), Normal "Ord"]
   no obligated docstring for this one! -}
data SynDef	= SynDef Name [Name] Type [TypeRequirement]

{-
> subtype Name = String -- see bnf for usage
> subtype TenSet (a in Ord)	= ...
no obligated docstring for this one! -}
data SubDef	= SubDef Name [Name] Type [TypeRequirement]

-- ## Creating classes and instances

-- Name: name of the new class; second Name: name of it in the functions; [(Name,Type)]: declarations
data ClassDef	= ClassDef
			{ name		:: Name
			, frees		:: [Name]
			, classReqs	:: [TypeRequirement]
			, subclassFrom	:: [Type]
			, classdocstr 	:: DocString
			, classlaws	:: [Law]
			, decls		:: [(Name,Type,Maybe Comment, [TypeRequirement])] }
	deriving (Ord, Eq)

data Instance	= Instance Name Type
