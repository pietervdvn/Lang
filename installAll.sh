#! /bin/bash

cd Consumer
cabal install --force
cd ../Regex
cabal install --force
cd ../Bnf
cabal install --force
cd ../Parser

