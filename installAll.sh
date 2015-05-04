# installs all subcomponents, in the right order

if [ "$1" = "" ]
then
	echo "Starting from Std/"
	START="Std/"
else
	START=$1
fi

FOUND=0
DONE=0

for pack in Std/ Graphs/ Consumer/ Regex/ Bnf/ Parser/ Loader/ MarkUp/ # SemanticAnalysis/
do
	DONE=$(($DONE+1))
	echo $DONE
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
			bell; bell; bell
			exit
		fi
	else
		echo "Skipping $pack"
	fi
done

bell 1
cd "SemanticAnalysis"
echo -e "bDocs\n:q" | ghci "Languate/MaintenanceAccess/TestBuild"
exit

cd Main
ghc Main.hs
EXITCODE="$?"
echo "Exit code $EXITCODE"
if [[ $EXITCODE -ne 0 ]]
then
	echo "Compiling langc (Main) failed"
	exit
fi

version="$(./Main --version)"
mv Main ../langc$version
rm *.hi
rm *.o
rm */*.hi
rm */*.o
cd ..
cd ..

# Test all the laws!
./langc$version --no-repl
