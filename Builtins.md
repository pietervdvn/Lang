Built-in stuff
==============

Built-in functions (used in desugaring)
------------------

    empty	: List a
    prepend	: a -> List a -> List a
    unprepend	: collection : Collection => collection a -> Maybe (a, collection a) -- used in patterns
    head	: [a] -> Maybe a
    just	: a -> Maybe a
    toSet	: List a -> Set a
    toDict	: List (a,b)	-> Dict (a,b)
    toString	: List Char -> String
    convert	: a -> b
    ~		-- cast
    ~~		-- autocast

List and Set are implemented as classes, the actual data structure is defined in languate and irrelevant.

'empty' and 'prepend' are called (automatically) to convert a list, Lists are desugared in the parser.

The 'toString' is to hide the fact that String is not a simple '[Char]'. String does implement '[Char]' (to support the list-functions) but it might implement thse more efficiently.

These methods are called with a special data structure in the AST, this is only because these can reach an other namespace.

Convert is used to add a conversion in the castin.

GetLonely is used in pattern matching, to match patterns as "[a]", "{a}". It should return Nothing if here are zero or multiple elements. Dicts should implement two versions (: {a --> b} -> Maybe a; {a --> b} -> Maybe (a,b) )

getLoneley list	= if isEmpty list then Nothing 
			else if isEmpty $ tail list 
				then Just head list
				else Nothing; 

just is used in pattern matching, to convert "(a,b) -> Maybe (a,b)" to deconstruct tuples. 

A constructor defines two functions: the constructor and deconstructor.

E.g.

    Either a b = Left a | Right b

instantatiates

    Left   : a -> Either a b
    Left   : Either a b -> Maybe a
    Right  : b -> Either a b
    Right  : Either a b -> Maybe b

### Tilde

Tilde acts as a cast (if convert is defined). E.g., for Bool is defined:


    convert	: Bool -> Nat
    True	= 1
    False	= 0

This means that

    ~ Nat True

is a valid expression and will yield 1.

It is even possible to cast directly to a float:

    ~ Float True

which will yield '1.0'. This is because '~' searches a path -via converts- from the actual typ to the target type.

Doubletilde is an operator which tries to infer the wanted type, and converts to that. One can thus easily write expressions as:

     5 * (~~ condition)

Builtin Types
-------------

There are some builtin types that should not be overwritten, as they have syntactic sugar.

- Void
- Nat
- Int
- Char
- Collection	-- supertype of List, Map and Set (and more?)
- Dict
- List
- Set
- Tuple
- String
- Maybe


Built-in functions (recognized by the interpreter)
------------------

-- used in constructors of adts, converts the arguments to an interpreter representation
#asADT	: Int -> a -> b -> c -> ... -> adt
-- Deconstructs an ADT. Always returns a Just. The int is the constructor number
#fromADT	: adt -> Maybe (Int, a, b, c, ...)
-- void if no args, id if one arg	
#asTuple	: a -> b -> c -> ... -> (a,b,c,...)
#plus
#mul
#mod
#div
#min
#showNat	: Nat -> ?
#showChar	: Nat -> ?



Functiondeclarations which contain a builtin type are considered as typecorrect
