#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=~/Desktop/Scratch/*.m4a
destination="illiac-local:~/Mass/Music"

for m4aFile in $files
do
    mp3File=~/Desktop/Scratch/"$(basename $(echo ${m4aFile} |sed -e 's/m4a/mp3/'))"
    mp3Base=$(basename $mp3File)
    echo "Converting $(basename $m4aFile) to $mp3Base"
    avconv -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File
    echo "Copying $mp3Base to $destination"
    scp $mp3File $destination
    echo "Deleting local copy of $mp3Base"
    rm $mp3File
done

IFS=$SAVEIFS
