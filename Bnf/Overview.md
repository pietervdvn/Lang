Overview
=========

This document describes how the package is constructed.

BNFP is a lib that can parse languages, based on defined bnf-files.

Parsing with a BNF-module object
--------------------------------

In bnf, you'll find (in ''Def'') the datastructures which represent BNF-files. With this description, you can parse an arbitrary string with 'parse' in the ''PtGen-module'', which generates a parsetree (''ParseTree'').

This parsetree can easily be converted into a whatever AST you want, using simpleConverter (''Converter'', take a look at these help functions) or with your own code.

Parsing a bnf-file
------------------

We have a BNF-parsing lib (which can parse arbitrary languages) and want to parse some language, named BNF? Hmmmm....

This module parses BNF-files following the above description. In ''Meta'', you will find manually compiled bnf-modules, which are used to convert a string (content of one ''.bnf'') into a 'IOModule'.
An IOModule represents a raw file, without resoved imports en checks.

IOModules are loaded with ''Bnf/Loader/Loader'', which searches and reads files to generate multiple ''IOModule''s. These raw modules are then converted into real modules using ''Loader/IOModule2Module''. Then, we have a BNFModule with which we can parse the desired language.
