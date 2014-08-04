Languate
========

Yet another programming language.

Languate aims to be a [simple](http://www.infoq.com/presentations/Simple-Made-Easy), functional programming language, highly inspired by Haskell; but with a more concise syntax. Once it will be finished, tooling will be included from the first run, so that documentation generation, testing, ... is included from the very start.

It tries to be haskell without a few syntactic quirks.

Code examples
=============

    map (1+) [1,2,3]
    [1,2,3].map(1+)
    
    -- docstring for function, the compiler automatically generates the docs; parsed in **markdown**
    > myFun 0 1 2   = 3     -- example embedded in the source code, acts as testcase (error if incorrect)
    ~ myFun with zero: myFun 0  = (+)       -- laws, checked by compiler; included in docs
    myFun   : Int -> Int -> Int
    0 a b   = a + b
    x a b   = x*a + b
    
    --- multiline
    comment
    with ---
    
    --## Literate programming features for docs

Getting started
===============

Install the Haskell platform (ghc+cabal) and mtl.

    sudo apt-get install ghc cabal-install
    cabal update
    cabal install mtl
    

Clone the repo and install all

    git clone git@github.com:pietervdvn/Lang.git
    cd Lang
    ./installAll


If you want to use the BNF-lib to  parse another languate, see the readme in bnf which contains a complete tutorial.


Repo structure
==============

Workspace
---------

Contains actual languate code!

StdDef
------

Some usefull functions, which where missing in the prelude.

Consumer
--------

A lightweight lib wich contains one (monstrous) monad which combines lots of things.
Is used in the regex and bnf parsing libs.

Regex
-----

A regex parsing/interpreting lib. Used withing the bnf-lib.

BNF
---

A bnf lib to load, parse and 'execute' bnf-files. See the readme in the bnf-dir for a tutorial.


Parser
------

Converts Strings into `Languate.AST`-data

Loader
------

Loads from file, checks imports and thus loads multiple sources at once. This 'cluster' is then ready for semantic analysis.

Semantic Analysis
-----------------

The next step in the compiler pipeline, where typechecking happens and unclear error messages live.

Interpreter
-----------

A simple program which executes 'compiled' programs.


Experiments
-----------

Haskell code experiments to try out concepts

Thoughts
--------

Each programming languate needs a blog!

Selling points
==============

* Haskell-like syntax (but cleaned up a little)
* Type Directed name resolution (ad hoc overloading, like in most OOP-languages)
* Type interference
* Laws, examples
