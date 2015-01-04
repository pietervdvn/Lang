#! /bin/bash

# Converts all .md's in workspace/Data/.gen to htmls

cd "workspace/Data/.gen"
for f in $(ls *.md)
do
	pandoc $f -f markdown_github -t html > ./html/$f.html
done
