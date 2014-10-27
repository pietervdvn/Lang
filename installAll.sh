#! /bin/bash

# installs all subcomponents, in the right order

if [ "$1" = "" ]
then
	echo "Starting from Std/"
	START="Std/"
else
	START=$1
fi

FOUND=0

for pack in Std/ Consumer/ Regex/ Bnf/ Parser/ Loader/ SemanticAnalysis/ Interpreter/ ExampleChecker/
do
	if [ "$FOUND" -eq 1 -o "$START" = "$pack" ]
	then
		FOUND=1
		echo ""
		echo "cabal install $pack --force"
		cabal install $pack --force
		EXITCODE="$?"
		echo "Exit code $EXITCODE"
		if [[ $EXITCODE -ne 0 ]]
		then
			echo "Installing $pack failed"
			exit
		fi
	else
		echo "Skipping $pack"
	fi
done

cd Main0
ghc Main.hs
if [[ $? -ne 0]]
then
	echo "Installing Main failed"
	exit
fi

mv Main ../Main
rm *.hi
rm *.o
rm */*.hi
rm */*.o
cd ..
./Main --no-repl
