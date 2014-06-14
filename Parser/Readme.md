PARSER
======

This module contains the actual data structure representing the languate language, alongside with the parser + its bnf files.

Overview
========


bnf
---

In bnf, there are bnf-files defining the syntax of the language. To test these files, open ````Test.hs```` in interactive mode, and run the function ````t "ruleToTest" "string to parse"````

Bugs and comments are welcome in the bug tracker (both about the syntax and the bnf lib)!

Def
---

Code will come there


workspace
---------

An example workspace with languate code. Acts as a example/regression test/tryout...

The subfiles in there (will) represent modules.

Feel free to add your own! (Once the code works :p)

Built-in functions (used in desugaring)
------------------

    empty	: List a
    prepend	: a -> List a -> List a
    toSet	: List a -> Set a
    toDict	: List (a,b)	-> Dict (a,b)
    toString	: List Char -> String
    ~		-- cast
    ~~		-- autocast

List and Set are implemented as classes, the actual data structure is defined in languate and irrelevant.

'empty' and 'prepend' are called (automatically) to convert a list, Lists are desugared in the parser.

The 'toString' is to hide the fact that String is not a simple '[Char]'. String does implement '[Char]' (to support the list-functions) but it might implement thse more efficiently.

These methods are called with a special data structure in the AST, this is only because these can reach an other namespace.


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
- Dict
- List
- Set
- Tuple
- String
- Maybe

