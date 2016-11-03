#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=~/Desktop/Scratch/*.m4a

for m4aFile in $files
do
    mp3File=~/Desktop/Scratch/"$(basename $(echo ${m4aFile} |sed -e 's/m4a/mp3/'))"
    echo $mp3File
    avconv -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File
    scp $mp3File illiac-local:~/Mass/Music
    rm $mp3File
done

IFS=$SAVEIFS
