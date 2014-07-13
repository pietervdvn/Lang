module Languate.AST where

import StdDef
import Data.Either
import Data.List
import Normalizable
{--

This module implements the data structures representing a parsed module. It is possible to recreate the source code starting from a datastructure like this (module some whitespace that's moved and a few dropped comments (but most should be preserverd)

The data flow is:

String -> parsetree -> cpt (often called ast in a pt2...) -> ast (here defined data structure) -> semantic analysis -> Interpreter

The structures here include comments (except those withing expressions); the data structures preserve order. These data structures are thus a good starting point for doc generation (but you'll have to sugar a little again, e.g. NormalType List -> [a], a:b:c:#empty -> [a,b,c] ...

--}



data Module	= Module {moduleName::Name, exports::Restrict, imports::Imports, statements::[Statement]}
	deriving (Show)

-- ## Stuf about imports

-- for comments hovering around imports
type Imports	= [Either Comment Import]

imports'	:: Module -> [Import]
imports' 	=  rights . imports


-- represents an import statement. public - Path - ModuleName- restrictions
data Import	= Import Visible [Name] Name Restrict
	deriving (Show)
-- restrict is the blacklist/whitelist of the showing/hiding in an import statement
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
	deriving (Show)


-- ## Things about function defitions

data Function	= Function DocString [(Name, Type)] [Law] [Clause]
	deriving (Show)

-- (Function docString decls laws clauses)

data Clause	= Clause [Pattern] Expression
instance Show Clause where
	show (Clause patterns expr)	= tabs 3 (unwords $ map show patterns)++"="++show expr

tabs	:: Int -> String -> String
tabs t str
	= str ++ replicate ((length str `div` 8) - t) '\t'



data Visible	= Private	
		| Public
	deriving (Show)


data Type	= Normal String	-- A normal type, e.g. Bool
		| Free String	-- A 'free' type, such as 'a', 'b'. (e.g. in id : a -> a)
		| Applied Type [Type]
		| Curry [Type]
		| TupleType [Type]
		| Infer
	deriving (Eq, Ord)

instance Normalizable Type where
	normalize	= nt


nt	:: Type -> Type
nt (Applied t [])	= nt t
nt (Applied t [t'])	= Applied (nt t) [nt t']
-- all applied types are written as ( (State Int) Bool) to make biding easier
-- > deriveBindings (m a) (State Int Bool) = [ (m, State Int), (a, Bool)
nt (Applied t ts)	= Applied (nt $ Applied t $ init ts) [nt $ last ts]
nt (Curry [t])		= nt t
nt (Curry ts)		= Curry [nt $ head ts, nt $ Curry $ tail ts ]
nt (TupleType [t])	= nt t
nt (TupleType ts)	= TupleType $ map nt ts
nt t			= t


traverse	:: (Type -> Type) -> Type -> Type
traverse f (Applied t ts)
		= Applied (traverse f t) $ map (traverse f) ts
traverse f (Curry ts)
		= Curry $ map (traverse f) ts
traverse f (TupleType ts)
		= TupleType $ map (traverse f) ts
traverse f t	= f t

instance Show Type where
	show t		= st t

st		:: Type -> String
st (Normal str)	=  str
st (Free str)	=  str
st (Applied t tps)
		=  "("++st t ++" "++ unwords (map st tps)++")"
st (Curry tps)	=  "("++intercalate " -> " (map st tps)++")"
st (TupleType tps)
		=  "(" ++ intercalate ", " (map st tps) ++")"
st Infer	= "_"


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

instance Show Expression where
	show	= se

se		:: Expression -> String
se (Nat i)	= show i
se (Flt flt)	= show flt
se (Chr c)	= show c
se (Seq expr)	= "("++unwords (map show expr) ++ ")"
se (Tuple exprs)
		= "("++intercalate ", " (map show exprs) ++ ")"
se (BuiltIn str)
		= '#':str
se (Cast t)	= "~("++show t++")"
se AutoCast	= "~~"
se (Call str)	= str
se (Operator str)
		= str
se _		= ""

data Pattern	= Assign Name	-- gives a name to the argument
		| Let Name Expression	-- evaluates the expression, and assigns it to the given name
		{- Deconstructs the argument with a function of type a -> Maybe (d,e,f,...), in which a is of the type of the corresponding argument.
			The patterns then match the tuple (there should be exactly the same number). If the maybe returns Nothing, matching fails
 				Map/Set/List syntactic sugar is translated into something like this. A constructor works in the opposite way here-}
		| Deconstruct Name [Pattern]
		| Multi [Pattern]	-- at-patterns
		| Eval Expression	-- evaluates an expression, which should equal the argument. Matching against ints is (secretly) done with this. 
		| DontCare		-- underscore

		| MultiDontCare		-- star
instance Show Pattern where
	show 	= sp

sp		:: Pattern -> String
sp (Assign nm)	=  nm
sp (Let nm expr)
		=  nm++":=("++show expr++")"
sp (Deconstruct nm patterns)
		= "("++nm++" "++ unwords (map show patterns)++")"
sp (Multi patterns)
		= intercalate "@" $ map show patterns
sp (Eval expr)	= '$':show expr
sp DontCare	= "_"
sp MultiDontCare
		= "*"


{- Deconstruct "unprepend" 
unprepend	: {a} -> (a,{a})
unprepend	: {a --> b} -> ( (a,b), {a --> b} )
unprepend dict	= ( head dict, tail dict)

thus, a pattern like

{a --> b, c : tail} gets translated to
Deconstruct "unprepend" [ Deconstruct "id" [Assign "a", Assign "b"],  Deconstruct "unprepend" [ Deconstruct "id" [Assign "c", Dontcare], Assign "tail"]
                          ^ splitting of key       ^ key        ^ value ^ deconstruct of tail                                 ^about the value  ^the actual tail, after 2 unprepends

-}

-- ### stuff 'around' function definitions

data Law	= Law Name [(Name, Maybe Type)] Expression Expression
		| Example Expression Expression
	deriving (Show)

type Comment	= String
-- a comment just before any declaration, (thus with no newlines in between)
type DocString	= Comment


-- # Type magic

-- ## New adts

{- data ADT	= A | B | C 

A single constructor is represented by a single ADTSum-object -}
data ADTSum	= ADTSum Name Visible (Maybe Comment) [(Maybe Name, Type)]
	deriving (Show)

-- data List a  = Cons a (List a) | Nil
-- becomes : ADTDef "List" ["a"] "Comment about a list" product
data ADTDef	= ADTDef Name [Name] DocString [ADTSum]
instance Show ADTDef where
	show (ADTDef name frees docstr sums)
		= "-- "++docstr++"\ndata "++name++foldr (\f acc -> f++" "++acc) " " frees ++ foldr (\s acc -> "\n\t"++show s++acc) "" sums


-- ## Synonym and subtyping

-- e.g. type Name = String
-- no obligated docstring for this one!
data SynDef	= SynDef Name [Name] Type
	deriving (Show)

-- e.g. subtype Name = String -- see bnf for usage
-- no obligated docstring for this one!
data SubDef	= SubDef Name [Name] Type
	deriving (Show)


-- ## Creating classes and instances

-- Name: name of the new class; second Name: name of it in the functions; [(Name,Type)]: declarations
data ClassDef	= ClassDef Name Name DocString [Law] [(Name,Type,Maybe Comment)]
	deriving (Show)
data Instance	= Instance Name Type
instance Show Instance where
	show (Instance name t)	= "instance "++name++" "++show t




