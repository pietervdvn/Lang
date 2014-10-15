#! /bin/bash

# installs all subcomponents, in the right order
cabal install Std/ --force && cabal install Consumer/ --force && cabal install Regex/ --force && cabal install Bnf/ --force && cabal install Parser/ --force && cabal install Loader/ --force && cabal install SemanticAnalysis/ --force && cabal install Interpreter/ --force

