#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=~/Desktop/Scratch/*.m4a

stripQuery() {
    echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//"
}

for m4aFile in $files
do
    # Converting to MP3
    mp3File=$(echo $m4aFile |sed -e 's/m4a/mp3/')
    echo $mp3File
    echo "Converting $m4aFile to $mp3File"
    avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File
    echo "Conversion successful!"
    rm $m4aFile

    # Tidying Up
    artistQuery="    artist          : "
    albumQuery="    album           : "
    albumArtistQuery="    album_artist    : "
    artist=$(stripQuery $mp3File $artistQuery)
    album=$(stripQuery $mp3File $albumQuery)
    albumArtist=$(stripQuery $mp3File $albumArtistQuery)
    albumFolder="/Users/Jack/Desktop/Scratch/$albumArtist/$album"

    echo $albumFolder
    if [ ! -e $albumFolder ]; then
        mkdir -p $albumFolder
        echo "Creating folder for $album"
    fi
    echo "Moving $mp3File to the correct folder"
    mv $mp3File $albumFolder/$(basename $mp3File)
    echo "Moving successful!"

done

IFS=$SAVEIFS
