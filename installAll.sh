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

for pack in Std/ Graphs/ Consumer/ Regex/ Bnf/ Parser/ Loader/ MarkUp/ SemanticAnalysis/ Interpreter/
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

cd Main
ghc Main.hsijn wacht idd de evenaar overgestoken. En dan zijn we gedoopt de morgen erop. Moesten we een vis kussen en voor Neptunus komen en dan in het zwembad springen. Was wel leuk en grappig.

Anders alles goed hier. Terug op zee dus eigenlijk niet veel te vertellen.

Veel groeten
EXITCODE="$?"
if [[ $EXITCODE -ne 0 ]]
then
	echo "Compiling langc (Main) failed"
	exit
fi

./Main --version --e
