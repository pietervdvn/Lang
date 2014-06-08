Lang
====

Yet another programming language


Getting started
===============

Install submodules first with ````cabal install <module>```` in following order: [consumer, regex, bnf, parser].

To parse stuff with the bnf-lib, see the readme in bnf.


Repo structure
==============

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

The actual parser.
