#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

musicPath=/mnt/usbStick/Music
#files=$(find $musicPath/ -name *.m4a)
files=$musicPath/*.mp3

function stripQuery {
    echo $(echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//") | sed -e "s/:/_/"
}

for mp3File in $files
do
    albumQuery="    album           : "
    albumArtistQuery="    album_artist    : "
    album=$(echo $(stripQuery $mp3File $albumQuery) | sed -e "s/:/_/")
    albumArtist=$(stripQuery $mp3File $albumArtistQuery)
    albumFolder="$musicPath/$albumArtist/$album"
    $mp3Base=$(basename $mp3File)
    if [ ! -e $albumFolder ]; then
        mkdir -p $albumFolder
        echo "Creating folder for $album at $albumFolder"
    fi
    echo "Moving $mp3Base to $albumFolder"
    mv $mp3File $albumFolder/$mp3Base
done

IFS=$SAVEIFS
