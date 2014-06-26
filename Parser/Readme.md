PARSER
======

This module contains the actual AST data structure representing the languate language, alongside with the parser + its bnf files.

This module is responsible of taking the input file and converting it to an AST. It contains the bnf-files, and parsers.


In this document:
Overview of the structure of the file
Common reasons code won't parse
Overview of builtin functions (which are desugared)

Overview
========


bnf
---

In bnf, there are bnf-files defining the syntax of the language. To test these files, open ````Test.hs```` in interactive mode, and run the function ````t "ruleToTest" "string to parse"````

Bugs and comments are welcome in the bug tracker (both about the syntax and the bnf lib)!

Def
---

Def.AST is the abstract syntax tree, which is a one-on-one mapping from the textual representation.
Def.Parser.Pt2* is code which parses a rule and converts it into an AST


workspace
---------

An example workspace with languate code. Acts as a example/regression test/tryout...

The subfiles in there (will) represent modules.

Feel free to add your own! (Once the code works :p)

Why doesn't my code parse?
==========================

- Check if each parenthese has a match: ````()````, ````[]````, ````{}````.
- Check if each class definition has a docstring
- Check if each module has a docstring
- Check if the class definition has a name:

    class Functor:	-- wrong, won't parse
    class Functor functor:	-- will work

- Still no success? File a bug


