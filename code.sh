#! /bin/bash

# Small script to open all kind of usefull applications
BROWSER="firefox"
./searchTodo.sh
cd SemanticAnalysis/ && n >/dev/null &
$BROWSER 'file:///home/pietervdvn/git/Lang/workspace/Data/.gen/html/All pages.html' >/dev/null &
$BROWSER 'github.com/pietervdvn/Lang/issues' >/dev/null &
ghci Languate/MaintenanceAccess/TestBuild.hs
