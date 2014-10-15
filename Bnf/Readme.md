BNFP is a parser library that uses bnf-files to generate a parser.

Contents:
- Usage overview
- Syntaxis
- Parsing

Usage:
======

* Define a BNF file/bnf file cluster
* Load the bnf with load. This will give you a map of type {fully qualified name --> module}
* Throw this world into PtGen.parse. (parse world (fqn of the module the rule is in) ("name of the rule you want to parse") "string to parse").
	You now have a parsetree.

* To convert this parsetree, you can use Converter.convert or simpleConvert, which takes three functions:
* - One that takes an rulename and a parsetree, and converts it into an cpt. This is the "hook" function.
* - One which tokenizes: it takes a rulename and content strings, and produces a ast
* - One which reduces sequences by taking a rulename and a sequence of asts.



BNF-Syntax
==========

Name
----

A bnf-file starts with it's fully qualified name: lowercase-starting strings, separated by dots, and the last one has an uppercase

    fully.qualified.Name

Metadata
--------

Then comes some metainfo. 
Obligated are 
- author: can be a string, or a list of strings
- date: at least 5 numbers
- desc: A description of what syntaxis is written in this bnf-file

	author	"Pieter Vander Vennet"
	date	[2014 05 21 13 58 42]
	desc	"The syntaxis of language ... "
    author	"pietervdvn"
    date	[2014 3 16 4 46]
    desc	"The syntax of a simple straight-line program; for the docs"

Strings are delimited by double quotes; lists are allowed and can be comma-seperated (but only a space is fine too); for date a list of (at least) 5 ints is expected in a year, month, day, hour, minute format.

Imports
-------

Then, imports follow:

    import OtherFile

imports all rules out of the OtherFile (except those marked private)

    public import OtherFile

will make sure that all rules of otherfile are exported too

    import OtherFile hiding {rule, rule2}

Now, all public things of OtherFile are imported, except for "rule" and "rule2". Again, comma-separators are optional.

    import OtherFile showing {rule, rule2}

Now **only** "rule" and "rule2" are visible.

Hiding and showing can be combined with public.

### Searching for other files

All files should have the extension ".bnf" (in small letters).

Lets say the FQN is "abc.def.Bar". The file should be placed in the directory "abc/def/Bar.bnf".
When an import happens, say "import ghi.Foo", the loader will first look for "abc/def/ghi/Foo.bnf", then in "abc/ghi/Foo.bnf", "ghi/Foo.bnf".
It will thus look from the closest place possible. People like to keep the word 'onion' in mind now.


Rules
-----

### Regexes

Regexes are double quoted.

All metachars are always metachars, use \ (backslash) if you need one.

sytnax:

* a	: literal a
* .	: any character
* *	: star: as much as possible
* +	: at least once
* ?	: optional
* r{i}	: regex r, exactly i times
* r{i,j}: regex r, between i and j times (both included). Both ints are optional
* a|b	: regex a or regex b. The longest will be prefered
* \.	: a literal dot
* [abc]	: class, one of a, b or c
* [a..z]: range: a char between (and including) a to z. We use .. as that is the functional way to write a list.
* !r	: not regex r. If r can be matched, no chars will be matched. If r cannot be matched, exactly as much chars will be matched as r would match.
* r1&r2	: both r1 and r2 have to be matched. The longest match will be returned in the parsetree

### Basic rules

Then: the rules. A basic rule will look like this:

	expr	::= int ("\+" expr) | "\(" expr "\)" | localIdent	-- do not forget the backslashes in the regexes!

Note that you can use "=" to instead of "::=" (but "::=" is better style).
Choices can be put on the next line, but *need* a *tab* indentation:

	stmt	= "print" expr*
		| localIdent ":=" expr

#### Modifiers

Our good old friends, * + and ? are included.

* With *, an expression is repeated as much as possible.
* With +, an expression is repeated as much as possible (but at least once)
* With ?, an expression is optional (parsed if possible)
* With % (prependend), the following expression is parsed without whitespace. This means that, ````%("a" "b")```` will equal ````"ab"````. Called rules still parse whitespace: ````a ::= "a" "b"; b ::= %("a" a)```` will parse ````aa b```` (but not ````a ab````)
* With $ (prependend), the following expression will be summoned into a single Token in the parsetree


There are also some other operators:

* { } (set) is used for when multiple rules should be matched once in random order. E.g:

	{rule1 | rule2 | rule3}

	will match rule1, rule3, rule2 just as well as rule3, rule2, rule1.

* & (ampersand, and) is used to parse the first rule optionally. E.g:

	rule1 & rule2 & rule3

    Will parse rule1 if and only if rule2 and rule3 could be parsed (from the same starting point). These rules are *not* included in the parse tree, only rule 1 is.

* ! (not), only to be used in combination with &
	This will reverse the and:
	
	rule1 & !rule2

	Will parse rule1 if and only if rule2 could *not* be parsed from the same starting point.


### Rule modifiers


Three modifiers can be used when declaring a rule:

	>initial	::= rule
	_private	::= rule
	$token		::= rule

* > makes a rule initial, this means that the parser will try to parse this rule as first. It is more of a human indication that it really gets used...
* _ makes a rule private; this means that when another bnf-file imports this bnf-file, this private rule will not be seen in its scope
* $ tokenizes a rule. This means that all the contents of the parsetree are concatenated to a single string. Note that white space is still parsed between the two rules.
	Usefull for e.g. localIdent:

	$localIdent	::= "[a..z]" "[a..zA..Z0..9]"*



Parsing order
-------------

The parser is a quite naive, backtracking parser. Left recursion will loop forever, so be carefull.
In what order will ptgen try to parse a string?

Lets say we have the following bnf

	rule	::= ident "::=" expr | stmt
	loop	::= loop "."

When parsing "rule", the parser will first (try to) parse ident, then to match the regex "::=" and then try to match expr.
If one of these fail, the parser will attempt to match stmt. If that fails too, the parser gives up and considers "rule" as failed.

Trying to parse "loop" is a bad idea. PtGen will try to parse loop. To do that, PtGen will try to parse loop. To do that, PtGen will try to parse loop.

Note that 

The actual parsing
==================

**This example is worked out completely in ````example````**.

Loading from IO
---------------

Loading from file is possible with ````load "file"````, with ````import Bnf````. This will give you a world object. This function will print a number, this is to deep evaluate the data and cause an exception if some things are incorrect.


Parsing a simple language
------------------------

e.g. You have a straight-line language that consist of assign and print statements (which both work on expressions). It only works with integers, and can only add them.

    a := 1+5
    b := a + 2
    print a
    print b


As the syntax is small, this easily fits in one file:

    localIdent	::= "[a..z][a..zA..Z]*"
    int		::= "[0..9]+"

    expr   		::= (int | localIdent) ( "\+" expr ) ?
    statement   	::= "print" expr | localIdent ":=" expr


Lets parse this!

Simple expressions
-------------------

To parse a simple expression, you define an AST. This AST is an intermediate representation. The idea is that we simplify it constantly as much as possible.

We do this with two functions: ````t```` (tokenize) converts single tokens of the pt into a token (which is AST too).

    data AST	= Value Int
		| Ident Name
		| Plus AST AST
		| PlusE AST
		| PlusT
	deriving (Show)

	t       :: Name -> String -> AST
	t _ "+"     =  PlusT
	t "int" i   = Value $ read i    -- read is a builtin function that parses int
	t "localIdent" name
		= Ident name

The other important function is ````s```` (simplify/sequence). It is called on each node, with the rulename that created the node and what is in that Pt.
Use it to simplify the node.

	s       :: Name -> [AST] -> AST
	s "expr" [expr1, PlusE expr2]
		= Plus expr1 expr2
	s "expr" [PlusT, expr]
		= PlusE expr
	s _ [expr]  = expr

Now we now these two functions, we can convert the parsetree with them:
	
	simpleConvert (const $ const Nothing) s t

````simpleConvert```` is a function that takes a parsetree and emits an AST (together with a bunch of errors/warnings, if there are any), by traversing the pt.


The AST is not the data structure that is used for the rest of the compiler pipeline. To represent an expression, we use the following data structure:

	data Expr	= Integer Int
			| Call Name	-- Name is another name for String
			| Add Expr Expr

To convert the AST into the Expr, we'll define a simple recursive function:

	conv		:: AST -> Expr
	conv (Value i)	= Integer i
	conv (Ident nm)	= Call nm
	conv (Plus e f)	= Add (conv e) (conv f)

Now we can parse an expression!

	parseExpr  	 :: ParseTree -> Writer Errors Expr
	parseExpr pt    =  do   parsed  <- simpleConvert (const $ const Nothing) t s pt
				let conved  = conv parsed
				    -- eventually, we can check certain properties here and issue a warning/error with tell. See Control.Monad.Writer docs
				return conved


Parsing statements, using hooks
-------------------------------

We'll define the statement AST as following:
	
	data AST	= Print [Expr]
			| Assign Name Expr
			| Expr Expr
			| PrintT
			| AssignT
			| Ident Name

To parse statements, we'll have to parse expressions. We'd love it to reuse the code to parse an expression, and don't want to clutter the AST-tree of a statement with "low-level" things of the expressions.
Wouldn't it be nice that, whenever we come accross an expression rule, we parse the expression (and keeps its warnings and errors) and include the expression into the AST, before the tokenizer and sequencer even see the (parts of) its parsetreee?

Thats exactly what the hooks are for. Hooks tell the converter "hey, this subtree, parse it with this function".

	h       :: Name -> ParseTree -> Maybe (Writer Errors AST)
	h "expr" pt = Just $ fmap Expr $ parseExpr pt
	h _ _       = Nothing

	t       :: Name -> String -> AST
	t _ ":="    =  AssignT
	t _ "print" =  PrintT
	t "localIdent" name
		=  Ident name

	s       :: Name -> [AST] -> AST
	s "statement" [PrintT, Expr e]
		=  Print e
	s "statement" [Ident nm, AssignT, Expr expr]
		=  Assign nm expr
	s _ [ast]
		= ast


Again, we'll need to convert from this messy AST into a specific data type for statements:

	data Statement	= PrintStmt Expr
			| AssignStmt Name Expr

	conv		:: AST -> Statement
	conv (Print expr)
			= PrintStmt expr
	conv (Assign name expr)
			= AssignStmt name expr

To actually parse, we can use

	parseStmt	:: ParseTree -> Writer Errors Statement
	parseStmt pt	=  do	parsed	<- simpleConvert h s t pt
				return $ conv parsed

Putting it together
-------------------

Note that all warnings and errors given by the expression parser, will be included when this monad is ran.
To run, use:

    -- notice that this parser parses only one statment
    parseLanguage	:: String -> (Statement, Errors)
    parseLanguage str
		= let parseTree	= parse world fqn "statement" str in    -- parses the string with the bnfs in world
	let parseTree'	= either (error $ "No parse result") id $ fromJust parseTree in	-- bit of unpacking
	fst $ runWriter $ parseStmt parseTree'

Errors is a list of type [Either Warning Error]. Both Warning and Error are tuples that contain all the ruleinfo (parsed according to what rule), position info in string (line + col number) and a textual message of what was expected.

What imports do I need?
-----------------------

    import StdDef	-- whenever you need 'Name', will be always
    import Bnf	-- always
    import Bnf.ParseTree	-- when you will do actual parsing and explicit parse tree manipulation
    import Bnf.Converter	-- when you need 'simpleConvert' or the 'Error'/'Errors'-data type 

Does the code above work?
-------------------------

Yes! See it in action in the ````example````-directory! Run ````Main.main```` and enjoy the show!
Or ````Main.parseTrees```` to see the parsetrees.
