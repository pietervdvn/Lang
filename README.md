Languate
========

Yet another programming language.

Languate aims to be a [simple](http://www.infoq.com/presentations/Simple-Made-Easy), functional programming language, highly inspired by Haskell; but with a more concise syntax. Once it will be finished, tooling will be included from the first run, so that documentation generation, testing, ... is included from the very start.

Code examples
=============

    map (1+) [1,2,3]
    [1,2,3].map(1+)
    

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

The next step in the compiler pipeline, where typechecking happens

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
