OUTPUT=`find | grep ".hs$" | grep -v "dist" | xargs hlint`
echo "$OUTPUT"


if [ "$1" == "open" ]
then
	echo "Opening stuff!"
	echo "$OUTPUT"  | grep "\./" | sed "s/\.\/\([^:]*\).*/\1/" | xargs g
fi
