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

cd Main
ghc Main.hs
<<<<<<< HEAD
<<<<<<< HEAD
version="$(./Main --version)"
mv Main ../langc$version
cd ..

./langc$version --no-repl
=======
if [[ $? -ne 0]]
then
	echo "Installing Main failed"
	exit
fi
=======
>>>>>>> 3a30b8de27c48b17b6a419b76bfd0b22a7b23eac

version="$(./Main --version)"
mv Main ../langc$version
rm *.hi
rm *.o
rm */*.hi
rm */*.o
cd ..
<<<<<<< HEAD
./Main --no-repl
>>>>>>> master
=======

./langc$version --no-repl

if [[ $? -ne 0]]
then
	echo "Installing Main failed"
	exit
fi
>>>>>>> 3a30b8de27c48b17b6a419b76bfd0b22a7b23eac
