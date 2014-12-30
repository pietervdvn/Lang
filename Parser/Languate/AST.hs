module Languate.AST (module AST)




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

import Languate.AST.FunctionAST as AST
import Languate.AST.FunctionASTUtils as AST
import Languate.AST.ModuleAST as AST
import Languate.AST.ModuleASTUtils as AST
import Languate.AST.TypeAST as AST
import Languate.AST.TypeASTUtils as AST
