#! /bin/bash

# installs all subcomponents, in the right order!

cabal install Consumer/ --force
cabal install Regex/ --force
cabal install Bnf/ --force
