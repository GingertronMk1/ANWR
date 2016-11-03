#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

musicPath=/mnt/usbStick/Music
#files=$(find $musicPath/ -name *.m4a)
files=$musicPath/*.mp3

stripQuery() {                                                                      # This is a long one. First it finds an instance of $2 in the output of avprobe
    $(echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//") | sed -e "s/:/_/"    # Then it strips out the $2 part of the line, so you just get the bit of metadata you want
}                                                                                   # Finally, it replaces all : with _, because we're using this for folder names and mkdir hates :

for mp3File in $files
do
    albumQuery="    album           : "
    albumArtistQuery="    album_artist    : "

    album=$(stripQuery $mp3File $albumQuery)
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
