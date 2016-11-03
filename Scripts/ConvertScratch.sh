#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=~/Desktop/Scratch/*.m4a

for m4aFile in $files
do
    # Converting to MP3
    mp3File=$(echo $m4aFile |sed -e 's/m4a/mp3/')
    echo Converting $m4aFile
    avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File
    echo Conversion finished!
    rm $m4aFile
done

IFS=$SAVEIFS
